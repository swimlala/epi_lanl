CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-04T21:42:42Z creation;2022-09-04T21:42:43Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220904214242  20220904215913  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��.7_1�1   @��.��d@.�I�^5?�c���Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B��B���B���B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C33C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C433C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Q�@xQ�@�(�@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�BpQ�Bw�B�B�B�B���B���B��]B���B���B���B�B���B���B���B���B���B���B���B�\)B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C{C.C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C4.C5�GC7�GC9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf{Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D#D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aد�Aر�Aر[Aز�AصAس�Aذ�Aز�Aز�Aظ�AطAض�Aط�Aط�AطAط�Aط�Aش�Aظ�AعXAش�Aس�Aر[Aر�AشAطAشA؟�A���Aך7A�k�AԵA��A��A�|�A�C-A˫�A˅�A���A�3�A��AƠ�A�]dA�zDA´A��A�Z�A���A��2A��LA�a�A��fA�A�%A��A���A���A�G�A�+A���A��A�D�A��A���A�GzA���A��0A���A���A��A���A�
=A�H�A�%zA�ƨA�̘A��XA��-A���A�U2A��A�e,A�/�A�.A�MjA�Y�A��]A�iyA�H�A���A��A�W�A~CAx8Ar�Amc�Ah��Af�\Ab�QAam]A_n/A\�hAY��AW�HATAO�oAKsAGACx�AA�A>��A<e,A9qA6-wA5<�A4�A2ĜA1��A0�!A/�cA/��A-�aA+�`A*�\A){JA(k�A'	lA'RTA'�DA&�"A$�dA"�A|A�+A��A��A�$AoiA�	A�KAԕA��A��AY�A�$A~(A�Ag8A��A�KAo�A�QAZ�A�9Af�Aw2Am�A	�A��A��A�LA�An/A<6A:�A�A��A�SAl�AیAl�A�oAaA(�A�/AB�A�TAa�A�wAA
z�A	�>A	��A	�AYKA�AGA�xA!�AS�AB[A:�A�6A��A��A��A�A��Ap;A��A ��A ��A ��A MA V@�G�@�h
@�l"@��W@���@��@��@�4n@��@���@�Q�@���@�l�@�@���@��@��@��B@�E�@��@�/@��r@�F@��"@�z@�S&@��)@�o@�@�*@�F�@�ȴ@�ں@��8@�8�@��@��W@�H@�Z@��@��Q@�"�@��@첖@��@�m]@�8@�D�@邪@��@��@�P�@���@�r@�E�@�X@� \@�@��@�ѷ@䠐@��@��@�!�@୬@��@�p;@߹�@�q@޽<@�x�@܊r@�?�@ۥ�@ڲ�@��T@�f�@��@�u�@�>B@�s�@؂A@���@֭�@�� @�o@��@ԟ�@�9X@Ӡ'@��@���@�{�@�M@���@�c�@��@�\�@��v@�6�@�A�@��M@̃�@���@˲�@�\)@���@�!�@ɟV@���@ȹ$@�J�@�~@��
@�m]@��f@��,@Ƒ�@�h�@�	@��@�.I@�u%@�,=@ó�@�%F@¨�@���@���@�[W@��@���@���@�n/@�N<@�0�@�&@�xl@�u�@��@��4@�y>@�z�@�3�@��0@���@�L0@��@���@�(@��,@���@�D�@��z@�a@��@���@��o@��&@�n/@��@�&�@�g�@���@�bN@��K@���@�*0@�c @��]@�b@�ff@�8�@���@�e,@�S@��m@�y>@�%�@�5?@��@���@�V@��@�H@�=@��U@�q@�~�@�E9@���@�2�@�~(@�%�@��[@��'@�$t@���@�}V@�!�@��@�ϫ@�rG@�q@���@���@�/@��@��<@��@�{J@�@��@��@��z@�  @���@�x@�@�Mj@��5@���@��@��+@�i�@�.�@�u@��a@�s�@�O�@��@��8@���@�J�@��3@��@�S&@��@��p@���@��@�"h@��]@���@���@�S�@���@��A@� �@���@�c�@��@�RT@�}�@��@�V@���@�~@���@��@��}@�}V@�H@�	�@���@�w2@�_p@�/�@���@�֡@���@�u%@�K^@���@���@�~�@�&�@���@���@��@�q@�Xy@�.�@��D@���@�`B@�/�@��@��f@��`@���@�{�@�Q�@�E�@�=q@��@��@���@���@���@���@�K�@��@��I@�Z@��o@���@�s�@�/@��I@�q@�Z�@��m@��@��F@��:@�f�@�8�@���@���@���@�z@�c�@�<�@�G@��"@�O�@��@��?@���@�U2@��@���@���@�a�@�Y@�%@�ں@���@�V@�*�@���@��&@��9@��z@���@���@�hs@�O�@��@���@��r@�=q@�@��@~�y@~xl@~#:@}�@|��@{s@z��@z-@y�@y�"@y�@x�@xH@wݘ@w�@@w�k@w�:@wC�@w@v��@v��@vO@u�S@tɆ@tg8@s��@s.I@s i@r��@r}V@r=q@r@q�S@p��@pPH@p/�@o�@o\)@oH�@o&@n�@nu%@nC�@m�C@m!�@m�@l�@kx@j�8@j��@i��@iw2@i	l@hɆ@h>B@g��@g�K@g�{@gMj@f��@f($@eVm@eV@d�P@d�)@d<�@d~@cݘ@cW?@c�@b�@bB[@a��@`��@`�@`*�@_��@_t�@_\)@_@O@_;d@^�<@^i�@]�n@]j@]X@\�v@\m�@[�F@Z�X@Z)�@Y�#@Y��@YDg@Y�@X��@XI�@X,=@Xb@W��@V�"@V�h@V�6@V�6@V=q@U�>@U��@UL�@U�@T�`@T��@T|�@T2�@Sݘ@S��@SA�@R��@R8�@R@Q��@Qf�@Q+�@P�@P|�@Pe�@PC-@O��@O��@O~�@O�@N�+@M��@M+�@LH@L�@K��@Kv`@K'�@J��@J��@J-@I�@Iw2@H�E@H9X@G�@GY@F��@Fi�@E��@E��@EO�@D�@DXy@C��@C�@C�$@C6z@Bxl@A��@A�=@A4@@��@@��@@Q�@@	�@?�m@?�w@?e�@? i@>�}@>��@>ff@>@=��@=�@<��@<Q�@;x@;�@:L0@9�n@9T�@9+�@9@8�U@8tT@8	�@7�:@7W?@7F�@7�@6��@6d�@6.�@5ԕ@5p�@5N<@4��@4~(@4M@4�@3��@3n/@3>�@2�@2�@2s�@23�@2�@1�)@1�z@1}�@1N<@1+@0�[@0�o@0A�@/�@/�6@/��@/��@/F�@/Y@.�@.�B@.�x@.n�@.e@-�@-w2@-*0@,�f@,�@+�@+��@+X�@+�@*�y@*��@*J�@*-@*O@)��@)�~@)f�@)�@(��@(y>@(U2@(-�@'�r@'ݘ@'��@'�4@'a@'J#@'.I@'Y@&��@&��@&��@&��@&\�@&J�@&B[@%�o@%��@%��@%�@%hs@%+�@$�?@$��@$��@$h�@$@#�F@#\)@#4�@#�@"�s@"0U@!�@!��@!�@!�@!#�@!@!�@ �E@ �U@ ��@ ~(@ w�@ c�@ 7@�@�;@��@��@��@n/@C�@�@ں@ȴ@��@�@5?@��@��@Vm@+@��@z�@<�@*�@M@�&@��@�k@_p@/�@��@�2@�H@�s@�B@�X@�m@��@��@($@��@w2@c�@L�@=�@*0@�U@��@��@��@��@��@_@!@�m@��@��@��@v`@l�@b�@F�@1�@��@kQ@u@�@�@��@a�@-w@V@��@��@��@Ĝ@��@�@?�@x@�r@��@v`@F�@�@�s@�h@�+@h
@M�@5?@#:@@��@��@�j@�3@��@�h@N<@L�@B�@-w@�@ی@�z@y>@]d@9X@!@�&@��@�:@n/@E9@�@�@�F@l�@ff@\�@:*@{@�D@��@�=@^�@�@�K@�/@��@g8@>B@�@��@�
@�0@��@=@�@
��@
z@
6�@	��@	��@	�@	x�@	j@	c�@	N<@	G�@	7L@	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aد�Aر�Aر[Aز�AصAس�Aذ�Aز�Aز�Aظ�AطAض�Aط�Aط�AطAط�Aط�Aش�Aظ�AعXAش�Aس�Aر[Aر�AشAطAشA؟�A���Aך7A�k�AԵA��A��A�|�A�C-A˫�A˅�A���A�3�A��AƠ�A�]dA�zDA´A��A�Z�A���A��2A��LA�a�A��fA�A�%A��A���A���A�G�A�+A���A��A�D�A��A���A�GzA���A��0A���A���A��A���A�
=A�H�A�%zA�ƨA�̘A��XA��-A���A�U2A��A�e,A�/�A�.A�MjA�Y�A��]A�iyA�H�A���A��A�W�A~CAx8Ar�Amc�Ah��Af�\Ab�QAam]A_n/A\�hAY��AW�HATAO�oAKsAGACx�AA�A>��A<e,A9qA6-wA5<�A4�A2ĜA1��A0�!A/�cA/��A-�aA+�`A*�\A){JA(k�A'	lA'RTA'�DA&�"A$�dA"�A|A�+A��A��A�$AoiA�	A�KAԕA��A��AY�A�$A~(A�Ag8A��A�KAo�A�QAZ�A�9Af�Aw2Am�A	�A��A��A�LA�An/A<6A:�A�A��A�SAl�AیAl�A�oAaA(�A�/AB�A�TAa�A�wAA
z�A	�>A	��A	�AYKA�AGA�xA!�AS�AB[A:�A�6A��A��A��A�A��Ap;A��A ��A ��A ��A MA V@�G�@�h
@�l"@��W@���@��@��@�4n@��@���@�Q�@���@�l�@�@���@��@��@��B@�E�@��@�/@��r@�F@��"@�z@�S&@��)@�o@�@�*@�F�@�ȴ@�ں@��8@�8�@��@��W@�H@�Z@��@��Q@�"�@��@첖@��@�m]@�8@�D�@邪@��@��@�P�@���@�r@�E�@�X@� \@�@��@�ѷ@䠐@��@��@�!�@୬@��@�p;@߹�@�q@޽<@�x�@܊r@�?�@ۥ�@ڲ�@��T@�f�@��@�u�@�>B@�s�@؂A@���@֭�@�� @�o@��@ԟ�@�9X@Ӡ'@��@���@�{�@�M@���@�c�@��@�\�@��v@�6�@�A�@��M@̃�@���@˲�@�\)@���@�!�@ɟV@���@ȹ$@�J�@�~@��
@�m]@��f@��,@Ƒ�@�h�@�	@��@�.I@�u%@�,=@ó�@�%F@¨�@���@���@�[W@��@���@���@�n/@�N<@�0�@�&@�xl@�u�@��@��4@�y>@�z�@�3�@��0@���@�L0@��@���@�(@��,@���@�D�@��z@�a@��@���@��o@��&@�n/@��@�&�@�g�@���@�bN@��K@���@�*0@�c @��]@�b@�ff@�8�@���@�e,@�S@��m@�y>@�%�@�5?@��@���@�V@��@�H@�=@��U@�q@�~�@�E9@���@�2�@�~(@�%�@��[@��'@�$t@���@�}V@�!�@��@�ϫ@�rG@�q@���@���@�/@��@��<@��@�{J@�@��@��@��z@�  @���@�x@�@�Mj@��5@���@��@��+@�i�@�.�@�u@��a@�s�@�O�@��@��8@���@�J�@��3@��@�S&@��@��p@���@��@�"h@��]@���@���@�S�@���@��A@� �@���@�c�@��@�RT@�}�@��@�V@���@�~@���@��@��}@�}V@�H@�	�@���@�w2@�_p@�/�@���@�֡@���@�u%@�K^@���@���@�~�@�&�@���@���@��@�q@�Xy@�.�@��D@���@�`B@�/�@��@��f@��`@���@�{�@�Q�@�E�@�=q@��@��@���@���@���@���@�K�@��@��I@�Z@��o@���@�s�@�/@��I@�q@�Z�@��m@��@��F@��:@�f�@�8�@���@���@���@�z@�c�@�<�@�G@��"@�O�@��@��?@���@�U2@��@���@���@�a�@�Y@�%@�ں@���@�V@�*�@���@��&@��9@��z@���@���@�hs@�O�@��@���@��r@�=q@�@��@~�y@~xl@~#:@}�@|��@{s@z��@z-@y�@y�"@y�@x�@xH@wݘ@w�@@w�k@w�:@wC�@w@v��@v��@vO@u�S@tɆ@tg8@s��@s.I@s i@r��@r}V@r=q@r@q�S@p��@pPH@p/�@o�@o\)@oH�@o&@n�@nu%@nC�@m�C@m!�@m�@l�@kx@j�8@j��@i��@iw2@i	l@hɆ@h>B@g��@g�K@g�{@gMj@f��@f($@eVm@eV@d�P@d�)@d<�@d~@cݘ@cW?@c�@b�@bB[@a��@`��@`�@`*�@_��@_t�@_\)@_@O@_;d@^�<@^i�@]�n@]j@]X@\�v@\m�@[�F@Z�X@Z)�@Y�#@Y��@YDg@Y�@X��@XI�@X,=@Xb@W��@V�"@V�h@V�6@V�6@V=q@U�>@U��@UL�@U�@T�`@T��@T|�@T2�@Sݘ@S��@SA�@R��@R8�@R@Q��@Qf�@Q+�@P�@P|�@Pe�@PC-@O��@O��@O~�@O�@N�+@M��@M+�@LH@L�@K��@Kv`@K'�@J��@J��@J-@I�@Iw2@H�E@H9X@G�@GY@F��@Fi�@E��@E��@EO�@D�@DXy@C��@C�@C�$@C6z@Bxl@A��@A�=@A4@@��@@��@@Q�@@	�@?�m@?�w@?e�@? i@>�}@>��@>ff@>@=��@=�@<��@<Q�@;x@;�@:L0@9�n@9T�@9+�@9@8�U@8tT@8	�@7�:@7W?@7F�@7�@6��@6d�@6.�@5ԕ@5p�@5N<@4��@4~(@4M@4�@3��@3n/@3>�@2�@2�@2s�@23�@2�@1�)@1�z@1}�@1N<@1+@0�[@0�o@0A�@/�@/�6@/��@/��@/F�@/Y@.�@.�B@.�x@.n�@.e@-�@-w2@-*0@,�f@,�@+�@+��@+X�@+�@*�y@*��@*J�@*-@*O@)��@)�~@)f�@)�@(��@(y>@(U2@(-�@'�r@'ݘ@'��@'�4@'a@'J#@'.I@'Y@&��@&��@&��@&��@&\�@&J�@&B[@%�o@%��@%��@%�@%hs@%+�@$�?@$��@$��@$h�@$@#�F@#\)@#4�@#�@"�s@"0U@!�@!��@!�@!�@!#�@!@!�@ �E@ �U@ ��@ ~(@ w�@ c�@ 7@�@�;@��@��@��@n/@C�@�@ں@ȴ@��@�@5?@��@��@Vm@+@��@z�@<�@*�@M@�&@��@�k@_p@/�@��@�2@�H@�s@�B@�X@�m@��@��@($@��@w2@c�@L�@=�@*0@�U@��@��@��@��@��@_@!@�m@��@��@��@v`@l�@b�@F�@1�@��@kQ@u@�@�@��@a�@-w@V@��@��@��@Ĝ@��@�@?�@x@�r@��@v`@F�@�@�s@�h@�+@h
@M�@5?@#:@@��@��@�j@�3@��@�h@N<@L�@B�@-w@�@ی@�z@y>@]d@9X@!@�&@��@�:@n/@E9@�@�@�F@l�@ff@\�@:*@{@�D@��@�=@^�@�@�K@�/@��@g8@>B@�@��@�
@�0@��@=@�@
��@
z@
6�@	��@	��@	�@	x�@	j@	c�@	N<@	G�@	7L@	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�DB
�*B
�*B
�*B
�yB
�KB
�*B
�DB
��B
��B
�KB
�B
�eB
�B
��B
�"B
��B
�'B
��B
�B
�1B
�\B�B:^BFYBIlB��B�MB�KB��B��B��B�ABB)B+kBQhBOBTB�iB�B��B�B�'B�@B��B�4Bt9Bf�B^jBP�BM�BG�BAoB5�B�B�B)BtB��B��B�B�B�nB�SB�]B�|B��B�B��Bp�BR�B%B�B
�uB
��B
u�B
LdB
8B
B	��B	�B	�B	�;B	��B	y�B	g�B	^�B	WYB	MPB	@ B	6�B	(�B	�B	�B	�B��B�hB�=B�DB�B��BΊB��B�GB��B��B��BżB�}B�rBΥBбB��B�WB�_B	(B	�B�"B��B�,B�jB�XB	mB	oB��B	-�B	P.B	X�B	\�B	eB	`�B	aB	[�B	^OB	a|B	_B	XEB	U2B	V9B	W�B	Y�B	Y�B	h$B	p�B	r�B	rGB	oiB	n/B	jB	j�B	p�B	}�B	��B	�_B	�_B	��B	�KB	��B	�_B	��B	��B	��B	�NB	��B	��B	��B	�WB	��B	�B	�)B	��B	��B	�yB	��B	�yB	��B	�)B	�OB	�;B	�VB	��B	��B	��B	��B	�`B	�zB	��B	��B	�mB	��B	�=B	�IB	��B	��B	�B	�tB	��B	��B	�0B	� B	�B	��B	��B	�B	�B	ɠB	ɆB	��B	ʦB	��B	��B	�"B	�B	бB	�B	�B	�TB	ңB	ԕB	ևB	ۦB	�B	��B	��B	�fB	�IB	��B	�	B	��B
  B	��B	�B	��B	�VB
 4B
�B	��B	��B	�]B	�jB	�JB	��B	�B	��B	�B	��B	��B	�B	��B	��B	�jB	�PB	��B	�dB	��B	��B	�B	�3B	�aB	�aB	�B	�B	�B	�B	��B	��B	�TB	�B	�GB	�B	�'B	�B	��B	��B	��B	�2B	�ZB	�B	�tB	�LB	��B	��B	��B	��B	�$B	�xB	�rB	��B	�9B	�9B	��B	�FB	��B	��B	��B	�>B	�B	��B	��B	�dB	�PB	��B	��B	�0B	��B	��B	��B	�}B	�HB	�}B
  B
 iB
 4B
 �B
 �B
�B
AB
�B
B	�}B	��B
 iB
 4B
;B
oB
�B
'B
'B
�B
B
�B
�B
�B
�B
{B
�B
B
B
�B
�B
?B
�B
tB
�B
�B
�B
YB
�B
�B
�B
tB
B
�B
tB
mB
B
B
�B
dB
�B
B
�B

#B
VB
dB
	B
�B
�B
	�B
	�B

�B
dB
B
�B
�B

�B
B
�B
�B
�B
B
dB
JB
DB
�B
B
JB
PB
PB
~B
�B
6B
"B
(B
pB
B
"B
.B
SB
B
�B
�B
B
�B
B
mB
B
�B
�B
�B
�B
HB
 B
B
4B
hB
�B
B
:B
�B
�B
�B
FB
FB
aB
�B
�B
�B
B
mB
�B
�B
�B
sB
YB
YB
?B
�B
yB
�B
KB
�B
	B
�B
�B
!�B
"�B
#TB
# B
!�B
�B
�B
IB
/B
B
IB
OB
OB
�B
;B
pB
pB
�B
�B
 B
 �B
 �B
 �B
!|B
!�B
!�B
!�B
"B
"B
"B
"NB
"�B
# B
#�B
#�B
#�B
$B
$tB
$�B
%FB
%,B
%FB
%zB
%�B
%�B
%�B
%�B
&LB
&�B
'B
'�B
($B
(�B
)*B
)yB
)�B
)�B
)�B
*eB
+B
+�B
+�B
,=B
,�B
,�B
-�B
-�B
.B
.IB
./B
.cB
.}B
/iB
/�B
0!B
0oB
0�B
0�B
1�B
2-B
2GB
2�B
2�B
3B
33B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4nB
4�B
5ZB
5tB
5�B
5�B
6B
6FB
6+B
6`B
6zB
72B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9	B
9XB
9XB
9XB
9>B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;B
;B
;�B
<B
<B
<B
<B
<B
;�B
<6B
<�B
<jB
<PB
=VB
=<B
=�B
>(B
?B
?}B
?.B
?cB
@4B
@�B
AoB
A�B
A;B
A;B
A�B
A�B
B'B
B�B
C�B
C{B
CaB
C�B
DB
DgB
EB
E9B
EB
EB
ESB
E�B
F%B
FYB
F�B
GEB
G�B
G�B
IB
IB
IB
I�B
I�B
J�B
J�B
K�B
L0B
L0B
L�B
L�B
MB
MB
MB
MB
M�B
N"B
NVB
N�B
N�B
O�B
O�B
P.B
P}B
P�B
P�B
P�B
Q B
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
SB
S[B
S[B
SuB
S�B
S�B
S�B
T,B
T,B
TaB
T�B
TaB
T�B
T�B
UMB
U�B
VB
V�B
V�B
WYB
WsB
W�B
W�B
W�B
X+B
X�B
X�B
X+B
YB
Y�B
Z7B
ZQB
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]/B
]dB
]~B
]�B
^B
^B
^B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
`\B
`vB
aHB
a�B
a�B
a�B
bB
bB
b�B
b�B
b�B
b�B
b�B
b�B
cTB
c:B
cTB
c�B
c�B
c�B
c�B
d@B
dZB
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
g8B
gRB
gRB
gRB
g�B
g�B
g�B
h
B
h
B
h>B
hsB
h�B
i*B
i_B
iyB
i�B
i�B
jB
jKB
j�B
j�B
kB
kQB
k6B
kQB
kQB
lB
lB
l"B
l=B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
nB
n/B
nIB
nIB
n}B
n}B
n�B
o5B
oB
oB
oiB
o�B
o�B
o�B
o�B
pB
pUB
poB
poB
p�B
p�B
qB
qvB
qvB
q�B
q�B
raB
r�B
r�B
r�B
r�B
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
tnB
t�B
tnB
t�B
t�B
u%B
utB
utB
u�B
u�B
u�B
v`B
v`B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
x�B
y	B
y$B
y	B
y>B
y>B
y>B
y>B
y$B
yXB
y�B
z^B
zxB
zxB
z�B
z�B
z�B
{B
{JB
{JB
{0B
{JB
{JB
{dB
{�B
{�B
|B
|B
|B
|6B
|PB
|6B
|PB
|6B
|�B
|�B
}"B
}VB
}qB
}qB
}�B
}�B
~B
~B
~B
~(B
~(B
~(B
~(B
~]B
~]B
~wB
~�B
.B
HB
}B
�B
�B
�B
� B
� B
�B
�4B
�4B
�OB
�iB
�iB
�iB
��B
��B
��B
��B
��B
��B
� B
�oB
��B
��B
��B
��B
�B
�[B
�uB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�mB
��B
��B
��B
��B
��B
�%B
�?B
�%B
�YB
��B
�B
�+B
�B
�+B
�EB
��B
��B
�KB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�DB
�*B
�*B
�*B
�yB
�KB
�*B
�DB
��B
��B
�KB
�B
�eB
�B
��B
�"B
��B
�'B
��B
�B
�1B
�\B�B:^BFYBIlB��B�MB�KB��B��B��B�ABB)B+kBQhBOBTB�iB�B��B�B�'B�@B��B�4Bt9Bf�B^jBP�BM�BG�BAoB5�B�B�B)BtB��B��B�B�B�nB�SB�]B�|B��B�B��Bp�BR�B%B�B
�uB
��B
u�B
LdB
8B
B	��B	�B	�B	�;B	��B	y�B	g�B	^�B	WYB	MPB	@ B	6�B	(�B	�B	�B	�B��B�hB�=B�DB�B��BΊB��B�GB��B��B��BżB�}B�rBΥBбB��B�WB�_B	(B	�B�"B��B�,B�jB�XB	mB	oB��B	-�B	P.B	X�B	\�B	eB	`�B	aB	[�B	^OB	a|B	_B	XEB	U2B	V9B	W�B	Y�B	Y�B	h$B	p�B	r�B	rGB	oiB	n/B	jB	j�B	p�B	}�B	��B	�_B	�_B	��B	�KB	��B	�_B	��B	��B	��B	�NB	��B	��B	��B	�WB	��B	�B	�)B	��B	��B	�yB	��B	�yB	��B	�)B	�OB	�;B	�VB	��B	��B	��B	��B	�`B	�zB	��B	��B	�mB	��B	�=B	�IB	��B	��B	�B	�tB	��B	��B	�0B	� B	�B	��B	��B	�B	�B	ɠB	ɆB	��B	ʦB	��B	��B	�"B	�B	бB	�B	�B	�TB	ңB	ԕB	ևB	ۦB	�B	��B	��B	�fB	�IB	��B	�	B	��B
  B	��B	�B	��B	�VB
 4B
�B	��B	��B	�]B	�jB	�JB	��B	�B	��B	�B	��B	��B	�B	��B	��B	�jB	�PB	��B	�dB	��B	��B	�B	�3B	�aB	�aB	�B	�B	�B	�B	��B	��B	�TB	�B	�GB	�B	�'B	�B	��B	��B	��B	�2B	�ZB	�B	�tB	�LB	��B	��B	��B	��B	�$B	�xB	�rB	��B	�9B	�9B	��B	�FB	��B	��B	��B	�>B	�B	��B	��B	�dB	�PB	��B	��B	�0B	��B	��B	��B	�}B	�HB	�}B
  B
 iB
 4B
 �B
 �B
�B
AB
�B
B	�}B	��B
 iB
 4B
;B
oB
�B
'B
'B
�B
B
�B
�B
�B
�B
{B
�B
B
B
�B
�B
?B
�B
tB
�B
�B
�B
YB
�B
�B
�B
tB
B
�B
tB
mB
B
B
�B
dB
�B
B
�B

#B
VB
dB
	B
�B
�B
	�B
	�B

�B
dB
B
�B
�B

�B
B
�B
�B
�B
B
dB
JB
DB
�B
B
JB
PB
PB
~B
�B
6B
"B
(B
pB
B
"B
.B
SB
B
�B
�B
B
�B
B
mB
B
�B
�B
�B
�B
HB
 B
B
4B
hB
�B
B
:B
�B
�B
�B
FB
FB
aB
�B
�B
�B
B
mB
�B
�B
�B
sB
YB
YB
?B
�B
yB
�B
KB
�B
	B
�B
�B
!�B
"�B
#TB
# B
!�B
�B
�B
IB
/B
B
IB
OB
OB
�B
;B
pB
pB
�B
�B
 B
 �B
 �B
 �B
!|B
!�B
!�B
!�B
"B
"B
"B
"NB
"�B
# B
#�B
#�B
#�B
$B
$tB
$�B
%FB
%,B
%FB
%zB
%�B
%�B
%�B
%�B
&LB
&�B
'B
'�B
($B
(�B
)*B
)yB
)�B
)�B
)�B
*eB
+B
+�B
+�B
,=B
,�B
,�B
-�B
-�B
.B
.IB
./B
.cB
.}B
/iB
/�B
0!B
0oB
0�B
0�B
1�B
2-B
2GB
2�B
2�B
3B
33B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4nB
4�B
5ZB
5tB
5�B
5�B
6B
6FB
6+B
6`B
6zB
72B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9	B
9XB
9XB
9XB
9>B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;B
;B
;�B
<B
<B
<B
<B
<B
;�B
<6B
<�B
<jB
<PB
=VB
=<B
=�B
>(B
?B
?}B
?.B
?cB
@4B
@�B
AoB
A�B
A;B
A;B
A�B
A�B
B'B
B�B
C�B
C{B
CaB
C�B
DB
DgB
EB
E9B
EB
EB
ESB
E�B
F%B
FYB
F�B
GEB
G�B
G�B
IB
IB
IB
I�B
I�B
J�B
J�B
K�B
L0B
L0B
L�B
L�B
MB
MB
MB
MB
M�B
N"B
NVB
N�B
N�B
O�B
O�B
P.B
P}B
P�B
P�B
P�B
Q B
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
SB
S[B
S[B
SuB
S�B
S�B
S�B
T,B
T,B
TaB
T�B
TaB
T�B
T�B
UMB
U�B
VB
V�B
V�B
WYB
WsB
W�B
W�B
W�B
X+B
X�B
X�B
X+B
YB
Y�B
Z7B
ZQB
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]/B
]dB
]~B
]�B
^B
^B
^B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
`\B
`vB
aHB
a�B
a�B
a�B
bB
bB
b�B
b�B
b�B
b�B
b�B
b�B
cTB
c:B
cTB
c�B
c�B
c�B
c�B
d@B
dZB
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
g8B
gRB
gRB
gRB
g�B
g�B
g�B
h
B
h
B
h>B
hsB
h�B
i*B
i_B
iyB
i�B
i�B
jB
jKB
j�B
j�B
kB
kQB
k6B
kQB
kQB
lB
lB
l"B
l=B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
nB
n/B
nIB
nIB
n}B
n}B
n�B
o5B
oB
oB
oiB
o�B
o�B
o�B
o�B
pB
pUB
poB
poB
p�B
p�B
qB
qvB
qvB
q�B
q�B
raB
r�B
r�B
r�B
r�B
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
tnB
t�B
tnB
t�B
t�B
u%B
utB
utB
u�B
u�B
u�B
v`B
v`B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
x�B
y	B
y$B
y	B
y>B
y>B
y>B
y>B
y$B
yXB
y�B
z^B
zxB
zxB
z�B
z�B
z�B
{B
{JB
{JB
{0B
{JB
{JB
{dB
{�B
{�B
|B
|B
|B
|6B
|PB
|6B
|PB
|6B
|�B
|�B
}"B
}VB
}qB
}qB
}�B
}�B
~B
~B
~B
~(B
~(B
~(B
~(B
~]B
~]B
~wB
~�B
.B
HB
}B
�B
�B
�B
� B
� B
�B
�4B
�4B
�OB
�iB
�iB
�iB
��B
��B
��B
��B
��B
��B
� B
�oB
��B
��B
��B
��B
�B
�[B
�uB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�mB
��B
��B
��B
��B
��B
�%B
�?B
�%B
�YB
��B
�B
�+B
�B
�+B
�EB
��B
��B
�KB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220904214234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220904214242  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220904214243  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220904214243                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220905064247  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220905064247  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220904215913                      G�O�G�O�G�O�                