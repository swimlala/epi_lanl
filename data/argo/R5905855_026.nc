CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:19Z creation;2022-06-04T19:15:20Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191519  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�����1   @���@0��C���d~��"�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B�  B���B�  B�  B�  B���B�33B�ffB�ffB���B�  B�33B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�33B�33B���B���B���B�  B�  C   C  C  C  C�C
�C�fC�fC�fC  C  C  C�C  C  C  C�fC!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C633C7�fC:  C<  C>�C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�(�B�B���B��]B���B���B���B��]B�(�B�\)B�\)B�B���B�(�B���B�\)B���B�B�B���B���B���B���B���B���B�(�B�(�B�B�B�B���B���B���C��C��C��C{C
{C�GC�GC�GC��C��C��C{C��C��C��C�GC!�GC#�GC%��C'��C)��C+��C-��C/��C1��C3��C6.C7�GC9��C;��C>{C?�GCA�GCC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��TA���A�oA�6FA�&�A� \A�=A��A��A��A�:A�4A�bA��A�PA��A�~A�xA�	�A�+A��A��A��A�uA���A���A��A��sA��A���Aߕ�A��XA��5A؅�A���A�MA�T�A�m�A͠\A�*0A�G�AƼ�AŕA�~�AÐ�A��A�m�A�A��wA��tA�.�A�+kA�m�A��dA�&�A��<A���A��2A���A�U�A�ޞA��(A�;0A�f2A���A�XyA�I�A�|A���A��A�t�A���A���A�iA�uA�_A�HKA��]A��A���A��A�!�A��A�]�A���A��^A��qA��tA�K^A�!-A~A}GAz�Ar�+Ag��A_p;AX�AUVAP8�ALzAKbAIRTAHC�AFh�AE iAD�AC �AA�qA?�{A>=qA=�sA<�kA:�QA9��A9�A:}VA9�YA7-A5�cA4��A2}�A0,=A/TaA+~(A*!�A*M�A(��A&��A%jA#�yA!�A5?A �A!�A"{JA!��A!	�A �1A ��A n/A �A�@A��A��A\)A2aA
�A�A�A!�AFA�]A!-A�}A��A`�A8�A�A|�A&A��A@OA��A�'A>�A�rAѷAZA iA��Ad�A%�A�DA�A�&AC-A�A}VA1�A�A+AS�AA�A��A��A�A]dA�hAl�A�AqA)�Ai�A
{JA
��A
A�A	�dA	|�A	�A�?A�nA,�A�jA�A�oA{�A�EA�A�A`BA��A�A��AS�A(�A��A ��A g8@�\)@�Dg@�$t@���@���@��r@�=q@���@�8@�0�@�V@���@�Z�@�1'@��c@��Z@��p@���@�?@�V@��[@��U@���@���@��@�hs@�+�@���@�1@���@�@�h@�֡@�~@��+@��D@�	l@��@�@�o@�@�5?@��@�S�@�6@�bN@�C-@�+k@��@��@��m@�˒@��@�%@�|�@�ϫ@�	l@���@�S�@�	@��@��@�,=@�M@��.@���@�	@�n/@�+@�
=@”@�H@�z@߽�@�?}@�C@�Ɇ@�V@�	@�ϫ@ݠ�@��@��@��H@��)@�m�@�!�@�l"@��@�(@��@�PH@י�@���@��@ֆY@�K^@�"h@��@ՠ'@�RT@��@ԩ�@�GE@���@ӎ�@Ҽj@���@�zx@�J�@�0�@���@�M�@���@΢4@�M�@�J�@�H�@��@ˠ'@�@�ȴ@ʁo@�9X@�@�ݘ@�˒@ɼ�@ɑh@�L�@�S@Ȅ�@���@�F�@� i@��y@�c @��8@ę1@�K^@���@��K@�_@�ԕ@��h@�B�@��@�v�@�kQ@�]d@�H�@��>@�c�@��@��j@��I@���@�H@�A @��@���@��4@�� @�rG@���@���@��@�0�@��@�oi@�7@���@�(@���@���@��@��O@�|�@��@��0@��{@��@�~�@�7@���@�j@���@�@�@���@�e�@�p�@�4�@���@�u�@��@���@��@���@�H�@�0U@��@��@�[W@�n�@��@��V@�Vm@��@�oi@��+@���@�l�@�|�@��h@��L@��+@���@�2a@��@��@��+@��^@��M@�:�@�#�@�%@��5@�ی@�>B@�ƨ@��@���@�k�@��	@��1@�M�@�u@��@�s@��@���@��u@�y>@�Ft@�x@��F@�c�@�33@��@�9X@��~@�8@���@��6@��_@�O@��~@�0�@��2@���@�"h@��d@�p�@�,�@�ߤ@��@�x@��>@��q@�\�@�*0@��5@��,@���@�2�@��W@��C@���@�hs@��@��R@��_@�)�@���@���@���@��V@���@�Z�@���@��F@�U2@��@��@���@�X�@� i@��@�h
@�@�@�8�@�	@��m@��3@��$@�G�@�Ɇ@��!@�z�@��]@��^@�e�@�7L@���@�p;@�B[@�$�@�7@�4@���@��w@�|@�#�@���@�0U@���@��'@�v`@��f@��@�ѷ@���@���@��U@��9@��A@�Q�@��&@��}@���@��K@��@� \@��c@��@�e�@�G@���@�T�@�8�@�F@�Y�@�A�@�.I@��@��|@���@�ff@�-@�
�@��W@��@��$@�a�@�Mj@��@��y@���@���@�M@��@��@_p@�@~��@~8�@}�t@|�f@|Z@| �@{�@{n/@{U�@z��@zں@z�@z	@y�o@y��@y��@y#�@x�?@xPH@w��@wO@w.I@v�c@v_�@uO�@u/@u&�@u(�@t��@t~(@t`�@s��@s�@sx@r�8@rv�@r3�@q�@qo @q@@p�_@p1'@o��@o��@oy�@n�F@nC�@m�D@m��@m�@l�?@l�@lr�@k�;@k{J@kH�@j�@j�B@j��@j3�@i�@irG@i+@h��@h�I@hK^@h�@g�a@gt�@gC�@g,�@f�@f#:@e�@ek�@e�@d��@c�4@cC@b��@b{@a�@aQ�@a@a%@`�@`�p@`��@`�Y@`V�@_ݘ@_�{@_9�@^�@^
�@]w2@\��@\��@\?�@\  @[��@[��@[H�@[@Z�}@ZR�@Y��@Y��@Yo @Y=�@X�@X9X@W��@WJ#@W'�@V��@V�2@V�'@V^5@V5?@U�Z@U��@Uf�@U�@T�@T�K@T�)@T?�@T�@S�W@Sqv@S@O@S�@R�@R�@R��@R6�@R�@Q�X@Q(�@P��@P[�@P"h@O�A@O�*@O i@N��@N@M��@M�@L��@Lx@K��@K!-@Jxl@J	@I��@I�@I��@I[W@I�@H�P@H��@H6@H2�@H�@G�[@GMj@G�@F��@F��@F�F@FH�@E��@E�C@EO�@D�@C��@Cg�@CS@B�L@Bu@A�9@A�'@Ap�@@��@@��@@�@@_@@7�@@%�@@M@?�Q@?��@?��@?iD@?RT@?�@>��@>��@>J�@>
�@=�@=0�@<�E@<�_@<�@<7�@;��@;�*@;dZ@:�@:��@:xl@:@�@9�)@9�M@9<6@8��@8r�@8Ft@7�@7��@7v`@7H�@6�@6�m@6��@6-@6	@5�T@5��@4�@4��@4h�@4<�@4�@3ƨ@3��@2�8@2{�@2=q@2!�@1��@1/@0�@0ی@0��@0u�@0M@/��@/S�@/(@.�]@.�x@.ff@-�@-Vm@,�?@,tT@,~@+�m@+�w@+��@+'�@*�H@*��@*��@*v�@*@�@)�@)�@)hs@)%F@(ی@(��@(�Y@(q@(D�@(�@'�@'�F@'b�@'�@&�,@&�@&z@&H�@&e@%��@%|@%Q�@%7L@%;@$�?@$�u@$c�@$:�@#�
@#��@#�@#S�@#33@"�@"��@"�@"R�@"u@!�@!��@!��@!-w@!%F@!%@ Ɇ@ Z@�+@��@��@8@�@�@��@��@��@R�@($@J@��@��@�@�@��@J�@#�@�@@@@��@�v@�U@��@-�@x@�@��@�m@� @�K@��@x@_p@U�@9�@S@ȴ@�A@-@�o@��@��@c�@�@��@��@y>@M@"h@�@� @�@x@K�@�@�X@��@Z�@�@��@�=@o @��@Ĝ@��@h�@H@�@�@��@�w@��@�:@E9@+@�@@�x@V@3�@J@�'@T�@�[@��@u�@h�@I�@�@��@��@9�@ߤ@͟11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��TA���A�oA�6FA�&�A� \A�=A��A��A��A�:A�4A�bA��A�PA��A�~A�xA�	�A�+A��A��A��A�uA���A���A��A��sA��A���Aߕ�A��XA��5A؅�A���A�MA�T�A�m�A͠\A�*0A�G�AƼ�AŕA�~�AÐ�A��A�m�A�A��wA��tA�.�A�+kA�m�A��dA�&�A��<A���A��2A���A�U�A�ޞA��(A�;0A�f2A���A�XyA�I�A�|A���A��A�t�A���A���A�iA�uA�_A�HKA��]A��A���A��A�!�A��A�]�A���A��^A��qA��tA�K^A�!-A~A}GAz�Ar�+Ag��A_p;AX�AUVAP8�ALzAKbAIRTAHC�AFh�AE iAD�AC �AA�qA?�{A>=qA=�sA<�kA:�QA9��A9�A:}VA9�YA7-A5�cA4��A2}�A0,=A/TaA+~(A*!�A*M�A(��A&��A%jA#�yA!�A5?A �A!�A"{JA!��A!	�A �1A ��A n/A �A�@A��A��A\)A2aA
�A�A�A!�AFA�]A!-A�}A��A`�A8�A�A|�A&A��A@OA��A�'A>�A�rAѷAZA iA��Ad�A%�A�DA�A�&AC-A�A}VA1�A�A+AS�AA�A��A��A�A]dA�hAl�A�AqA)�Ai�A
{JA
��A
A�A	�dA	|�A	�A�?A�nA,�A�jA�A�oA{�A�EA�A�A`BA��A�A��AS�A(�A��A ��A g8@�\)@�Dg@�$t@���@���@��r@�=q@���@�8@�0�@�V@���@�Z�@�1'@��c@��Z@��p@���@�?@�V@��[@��U@���@���@��@�hs@�+�@���@�1@���@�@�h@�֡@�~@��+@��D@�	l@��@�@�o@�@�5?@��@�S�@�6@�bN@�C-@�+k@��@��@��m@�˒@��@�%@�|�@�ϫ@�	l@���@�S�@�	@��@��@�,=@�M@��.@���@�	@�n/@�+@�
=@”@�H@�z@߽�@�?}@�C@�Ɇ@�V@�	@�ϫ@ݠ�@��@��@��H@��)@�m�@�!�@�l"@��@�(@��@�PH@י�@���@��@ֆY@�K^@�"h@��@ՠ'@�RT@��@ԩ�@�GE@���@ӎ�@Ҽj@���@�zx@�J�@�0�@���@�M�@���@΢4@�M�@�J�@�H�@��@ˠ'@�@�ȴ@ʁo@�9X@�@�ݘ@�˒@ɼ�@ɑh@�L�@�S@Ȅ�@���@�F�@� i@��y@�c @��8@ę1@�K^@���@��K@�_@�ԕ@��h@�B�@��@�v�@�kQ@�]d@�H�@��>@�c�@��@��j@��I@���@�H@�A @��@���@��4@�� @�rG@���@���@��@�0�@��@�oi@�7@���@�(@���@���@��@��O@�|�@��@��0@��{@��@�~�@�7@���@�j@���@�@�@���@�e�@�p�@�4�@���@�u�@��@���@��@���@�H�@�0U@��@��@�[W@�n�@��@��V@�Vm@��@�oi@��+@���@�l�@�|�@��h@��L@��+@���@�2a@��@��@��+@��^@��M@�:�@�#�@�%@��5@�ی@�>B@�ƨ@��@���@�k�@��	@��1@�M�@�u@��@�s@��@���@��u@�y>@�Ft@�x@��F@�c�@�33@��@�9X@��~@�8@���@��6@��_@�O@��~@�0�@��2@���@�"h@��d@�p�@�,�@�ߤ@��@�x@��>@��q@�\�@�*0@��5@��,@���@�2�@��W@��C@���@�hs@��@��R@��_@�)�@���@���@���@��V@���@�Z�@���@��F@�U2@��@��@���@�X�@� i@��@�h
@�@�@�8�@�	@��m@��3@��$@�G�@�Ɇ@��!@�z�@��]@��^@�e�@�7L@���@�p;@�B[@�$�@�7@�4@���@��w@�|@�#�@���@�0U@���@��'@�v`@��f@��@�ѷ@���@���@��U@��9@��A@�Q�@��&@��}@���@��K@��@� \@��c@��@�e�@�G@���@�T�@�8�@�F@�Y�@�A�@�.I@��@��|@���@�ff@�-@�
�@��W@��@��$@�a�@�Mj@��@��y@���@���@�M@��@��@_p@�@~��@~8�@}�t@|�f@|Z@| �@{�@{n/@{U�@z��@zں@z�@z	@y�o@y��@y��@y#�@x�?@xPH@w��@wO@w.I@v�c@v_�@uO�@u/@u&�@u(�@t��@t~(@t`�@s��@s�@sx@r�8@rv�@r3�@q�@qo @q@@p�_@p1'@o��@o��@oy�@n�F@nC�@m�D@m��@m�@l�?@l�@lr�@k�;@k{J@kH�@j�@j�B@j��@j3�@i�@irG@i+@h��@h�I@hK^@h�@g�a@gt�@gC�@g,�@f�@f#:@e�@ek�@e�@d��@c�4@cC@b��@b{@a�@aQ�@a@a%@`�@`�p@`��@`�Y@`V�@_ݘ@_�{@_9�@^�@^
�@]w2@\��@\��@\?�@\  @[��@[��@[H�@[@Z�}@ZR�@Y��@Y��@Yo @Y=�@X�@X9X@W��@WJ#@W'�@V��@V�2@V�'@V^5@V5?@U�Z@U��@Uf�@U�@T�@T�K@T�)@T?�@T�@S�W@Sqv@S@O@S�@R�@R�@R��@R6�@R�@Q�X@Q(�@P��@P[�@P"h@O�A@O�*@O i@N��@N@M��@M�@L��@Lx@K��@K!-@Jxl@J	@I��@I�@I��@I[W@I�@H�P@H��@H6@H2�@H�@G�[@GMj@G�@F��@F��@F�F@FH�@E��@E�C@EO�@D�@C��@Cg�@CS@B�L@Bu@A�9@A�'@Ap�@@��@@��@@�@@_@@7�@@%�@@M@?�Q@?��@?��@?iD@?RT@?�@>��@>��@>J�@>
�@=�@=0�@<�E@<�_@<�@<7�@;��@;�*@;dZ@:�@:��@:xl@:@�@9�)@9�M@9<6@8��@8r�@8Ft@7�@7��@7v`@7H�@6�@6�m@6��@6-@6	@5�T@5��@4�@4��@4h�@4<�@4�@3ƨ@3��@2�8@2{�@2=q@2!�@1��@1/@0�@0ی@0��@0u�@0M@/��@/S�@/(@.�]@.�x@.ff@-�@-Vm@,�?@,tT@,~@+�m@+�w@+��@+'�@*�H@*��@*��@*v�@*@�@)�@)�@)hs@)%F@(ی@(��@(�Y@(q@(D�@(�@'�@'�F@'b�@'�@&�,@&�@&z@&H�@&e@%��@%|@%Q�@%7L@%;@$�?@$�u@$c�@$:�@#�
@#��@#�@#S�@#33@"�@"��@"�@"R�@"u@!�@!��@!��@!-w@!%F@!%@ Ɇ@ Z@�+@��@��@8@�@�@��@��@��@R�@($@J@��@��@�@�@��@J�@#�@�@@@@��@�v@�U@��@-�@x@�@��@�m@� @�K@��@x@_p@U�@9�@S@ȴ@�A@-@�o@��@��@c�@�@��@��@y>@M@"h@�@� @�@x@K�@�@�X@��@Z�@�@��@�=@o @��@Ĝ@��@h�@H@�@�@��@�w@��@�:@E9@+@�@@�x@V@3�@J@�'@T�@�[@��@u�@h�@I�@�@��@��@9�@ߤ@͟11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	p�B	p;B	p!B	o�B	oB	o�B	o�B	o�B	p!B	p;B	p;B	p!B	pUB	pUB	poB	poB	p�B	p�B	p�B	p�B	p�B	p�B	p�B	p�B	q'B	qAB	q[B	q�B	r�B	u�B	}B	��B	��B	��B	��B	x�B	oB	p�B	��B	��B	��B	�"B	�B
A B
jeB
y�B
�-B
��B
�zB
�SB
�B
�5B
�rB
��B
ȀB
�"B
�=B
��B
�QB
ٚB(B��B�0B��B�~B�/B��B��B��B�YB�uB�EB}qBe�BDB�B
�B
�_B
kB
Z�B
T�B
MPB
DMB
4B
/�B
)B
�B
�B	�[B	�/B	ɠB	��B	�wB	��B	��B	xlB	HKB	$�B		RB�B�DB�B��B��B�B��B�B�B�B��B�)BخB�qB�YB�
B��B�B	  B	�B	
	B	JB	EB	 �B�MB�9B�BބB�$B��B��B�B�B�fB��B��B	4�B	h�B	}�B	�B	�9B	��B	��B	��B	�B	��B	�}B	�&B	��B	�gB	�9B	�KB	�B	��B	��B	�B	��B	��B	�"B	��B	�B	��B	��B	�iB	��B	��B	��B	��B	�2B	�DB	��B	�[B	��B	ƎB	�_B	ǮB	��B	�GB	��B	�6B	�(B	ЗB	�SB	�bB	�8B	�B	�*B	�B	�RB	�AB	�B	��B	��B	�B	��B	��B	��B	��B	�B
 4B
  B
�B
�B
�B
�B
YB
�B	��B	�?B	�B	��B	�B	�B	��B	�CB	��B	�B	�B	�=B	�B	�qB	�B	�B	�B	�B	�8B	�B	��B	�B	��B	�B	�B	�B	�B	��B	�LB	�FB	�3B	��B	�+B	��B
B
 B
 �B	�HB	��B
�B
�B	��B	��B	�dB	��B	�fB	�B	�iB	�'B	�9B	�vB	�oB	��B	�B	�B	�]B	�B	��B	�]B	�CB	�CB	�wB	�/B	�iB	�!B	�B	�;B	�B	�-B	��B	��B	��B	�/B	�B	�B	�/B	�iB	�B	��B	�B	��B	�oB	�B	��B	�'B	�B	�B	�/B	�}B	��B	�UB	�;B	�B	�B	��B	�nB	�nB	�9B	�B	�TB	��B	��B	�%B	�tB	�%B	��B	�+B	�B	��B	�FB	�FB	�+B	��B	��B	�`B	��B	�ZB	��B	�B	��B	��B	�B	��B	�oB	�B	��B	��B	�UB	�5B	�cB	�B	�B	��B	��B	�oB	�oB	��B	�oB	�;B	�;B	�;B	�B	�B	�UB	��B	�'B	�B	�B	��B	�[B	�oB	��B	�B	�B	��B	�"B	�)B	��B	��B	�cB	�B	�OB	� B	��B	�B	�B	�/B	�}B	�5B	�B	��B	�B	�?B	�?B	��B	�nB	�B	�9B	�B	��B	�zB	��B	��B	��B	�B	��B	�B	�B	�RB	��B	��B	��B	��B	��B	�rB	��B	�B	��B	�B	�B	��B	��B	�2B	�8B	�>B	�rB	�rB	��B	�*B	�B	�*B	��B	��B	�]B	�]B	�wB
 �B
AB
�B
aB
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
?B
tB
�B
zB
�B
�B
�B
�B
	B
	B
	�B

=B

XB

XB
)B
�B
�B
xB
�B
�B
B
JB
�B
B
jB
�B
�B
.B
�B
�B
 B
�B
�B
�B
 B
TB
oB
oB
�B
@B
uB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
+B
B
+B
�B
�B
+B
EB
sB
?B
YB
$B
?B
?B
YB
�B
�B
�B
�B
sB
$B
�B
�B
�B
�B
+B
B
�B
�B
B
EB
�B
�B
eB
�B
�B
�B
�B
)B
B
jB
�B
 BB
 vB
!B
!-B
!�B
#�B
%B
%FB
&2B
%�B
&2B
&2B
&LB
%�B
%�B
%�B
&B
&�B
'�B
(�B
)_B
)yB
)yB
)DB
)B
(�B
(�B
)DB
)�B
)�B
)�B
)�B
*KB
+B
+B
+6B
-)B
.cB
.�B
.�B
.�B
/ B
.�B
.IB
./B
.�B
.�B
/B
/5B
/B
/iB
/�B
/�B
0B
0�B
0�B
0�B
1[B
1[B
1�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
3�B
4TB
4TB
4nB
49B
4nB
4�B
4�B
4�B
5B
5ZB
5tB
5�B
6B
6B
6FB
6FB
7fB
7�B
7�B
7�B
7�B
8B
8B
8B
8�B
9$B
9>B
9rB
9�B
9�B
9�B
:xB
:�B
;B
;dB
;0B
;JB
;B
<B
<6B
<jB
<jB
<�B
=VB
=�B
=�B
>]B
>�B
?�B
?�B
@iB
@�B
A B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
BuB
B[B
B'B
B[B
B�B
B�B
CGB
C�B
C�B
D3B
D3B
DMB
D�B
D�B
ESB
E�B
E�B
E�B
FB
E�B
FB
F?B
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HB
HB
HKB
HfB
H�B
IB
IB
IB
IlB
I�B
I�B
I�B
I�B
J	B
JXB
JXB
JrB
J�B
J�B
K)B
KDB
KDB
KDB
KxB
KxB
L0B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
M6B
M6B
MjB
MjB
MjB
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
O(B
O\B
O\B
O�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
QB
Q4B
Q�B
Q�B
R�B
RoB
R�B
R�B
SB
S@B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
TFB
TFB
TaB
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
VmB
V�B
V�B
V�B
W?B
WYB
W�B
W�B
W�B
X_B
XyB
X�B
X�B
YB
YKB
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
[qB
[WB
[�B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
\�B
]/B
^B
^5B
^5B
^jB
^�B
^�B
_VB
_�B
_�B
_�B
`'B
`'B
`�B
aB
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
b�B
c B
b�B
cnB
c�B
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
e`B
e�B
f2B
f2B
ffB
ffB
gB
gB
gRB
gmB
g�B
g�B
h
B
h$B
h>B
h�B
h�B
h�B
i*B
iDB
i_B
i*B
iyB
jB
jKB
jKB
jeB
jeB
jKB
jeB
j�B
j�B
j�B
jeB
j0B
jKB
j0B
i�B
j0B
jKB
j�B
k�B
k6B
kQB
k�B
lB
l=B
lWB
l�B
mB
mCB
m]B
m]B
m]B
m]B
mwB
m�B
m�B
m�B
n�B
o B
o B
o B
oB
o B
n�B
n�B
n�B
o5B
oB
o5B
oOB
o�B
pB
pUB
p�B
p�B
p�B
qB
q'B
q�B
q�B
q�B
q�B
rB
q�B
rGB
r�B
r|B
r�B
r�B
r�B
sB
shB
s�B
s�B
tB
tB
t�B
t�B
uB
u%B
u?B
utB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v�B
v�B
v�B
v�B
wLB
wB
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
xRB
xlB
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	p�B	p;B	p!B	o�B	oB	o�B	o�B	o�B	p!B	p;B	p;B	p!B	pUB	pUB	poB	poB	p�B	p�B	p�B	p�B	p�B	p�B	p�B	p�B	q'B	qAB	q[B	q�B	r�B	u�B	}B	��B	��B	��B	��B	x�B	oB	p�B	��B	��B	��B	�"B	�B
A B
jeB
y�B
�-B
��B
�zB
�SB
�B
�5B
�rB
��B
ȀB
�"B
�=B
��B
�QB
ٚB(B��B�0B��B�~B�/B��B��B��B�YB�uB�EB}qBe�BDB�B
�B
�_B
kB
Z�B
T�B
MPB
DMB
4B
/�B
)B
�B
�B	�[B	�/B	ɠB	��B	�wB	��B	��B	xlB	HKB	$�B		RB�B�DB�B��B��B�B��B�B�B�B��B�)BخB�qB�YB�
B��B�B	  B	�B	
	B	JB	EB	 �B�MB�9B�BބB�$B��B��B�B�B�fB��B��B	4�B	h�B	}�B	�B	�9B	��B	��B	��B	�B	��B	�}B	�&B	��B	�gB	�9B	�KB	�B	��B	��B	�B	��B	��B	�"B	��B	�B	��B	��B	�iB	��B	��B	��B	��B	�2B	�DB	��B	�[B	��B	ƎB	�_B	ǮB	��B	�GB	��B	�6B	�(B	ЗB	�SB	�bB	�8B	�B	�*B	�B	�RB	�AB	�B	��B	��B	�B	��B	��B	��B	��B	�B
 4B
  B
�B
�B
�B
�B
YB
�B	��B	�?B	�B	��B	�B	�B	��B	�CB	��B	�B	�B	�=B	�B	�qB	�B	�B	�B	�B	�8B	�B	��B	�B	��B	�B	�B	�B	�B	��B	�LB	�FB	�3B	��B	�+B	��B
B
 B
 �B	�HB	��B
�B
�B	��B	��B	�dB	��B	�fB	�B	�iB	�'B	�9B	�vB	�oB	��B	�B	�B	�]B	�B	��B	�]B	�CB	�CB	�wB	�/B	�iB	�!B	�B	�;B	�B	�-B	��B	��B	��B	�/B	�B	�B	�/B	�iB	�B	��B	�B	��B	�oB	�B	��B	�'B	�B	�B	�/B	�}B	��B	�UB	�;B	�B	�B	��B	�nB	�nB	�9B	�B	�TB	��B	��B	�%B	�tB	�%B	��B	�+B	�B	��B	�FB	�FB	�+B	��B	��B	�`B	��B	�ZB	��B	�B	��B	��B	�B	��B	�oB	�B	��B	��B	�UB	�5B	�cB	�B	�B	��B	��B	�oB	�oB	��B	�oB	�;B	�;B	�;B	�B	�B	�UB	��B	�'B	�B	�B	��B	�[B	�oB	��B	�B	�B	��B	�"B	�)B	��B	��B	�cB	�B	�OB	� B	��B	�B	�B	�/B	�}B	�5B	�B	��B	�B	�?B	�?B	��B	�nB	�B	�9B	�B	��B	�zB	��B	��B	��B	�B	��B	�B	�B	�RB	��B	��B	��B	��B	��B	�rB	��B	�B	��B	�B	�B	��B	��B	�2B	�8B	�>B	�rB	�rB	��B	�*B	�B	�*B	��B	��B	�]B	�]B	�wB
 �B
AB
�B
aB
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
?B
tB
�B
zB
�B
�B
�B
�B
	B
	B
	�B

=B

XB

XB
)B
�B
�B
xB
�B
�B
B
JB
�B
B
jB
�B
�B
.B
�B
�B
 B
�B
�B
�B
 B
TB
oB
oB
�B
@B
uB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
+B
B
+B
�B
�B
+B
EB
sB
?B
YB
$B
?B
?B
YB
�B
�B
�B
�B
sB
$B
�B
�B
�B
�B
+B
B
�B
�B
B
EB
�B
�B
eB
�B
�B
�B
�B
)B
B
jB
�B
 BB
 vB
!B
!-B
!�B
#�B
%B
%FB
&2B
%�B
&2B
&2B
&LB
%�B
%�B
%�B
&B
&�B
'�B
(�B
)_B
)yB
)yB
)DB
)B
(�B
(�B
)DB
)�B
)�B
)�B
)�B
*KB
+B
+B
+6B
-)B
.cB
.�B
.�B
.�B
/ B
.�B
.IB
./B
.�B
.�B
/B
/5B
/B
/iB
/�B
/�B
0B
0�B
0�B
0�B
1[B
1[B
1�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
3�B
4TB
4TB
4nB
49B
4nB
4�B
4�B
4�B
5B
5ZB
5tB
5�B
6B
6B
6FB
6FB
7fB
7�B
7�B
7�B
7�B
8B
8B
8B
8�B
9$B
9>B
9rB
9�B
9�B
9�B
:xB
:�B
;B
;dB
;0B
;JB
;B
<B
<6B
<jB
<jB
<�B
=VB
=�B
=�B
>]B
>�B
?�B
?�B
@iB
@�B
A B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
BuB
B[B
B'B
B[B
B�B
B�B
CGB
C�B
C�B
D3B
D3B
DMB
D�B
D�B
ESB
E�B
E�B
E�B
FB
E�B
FB
F?B
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HB
HB
HKB
HfB
H�B
IB
IB
IB
IlB
I�B
I�B
I�B
I�B
J	B
JXB
JXB
JrB
J�B
J�B
K)B
KDB
KDB
KDB
KxB
KxB
L0B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
M6B
M6B
MjB
MjB
MjB
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
O(B
O\B
O\B
O�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
QB
Q4B
Q�B
Q�B
R�B
RoB
R�B
R�B
SB
S@B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
TFB
TFB
TaB
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
VmB
V�B
V�B
V�B
W?B
WYB
W�B
W�B
W�B
X_B
XyB
X�B
X�B
YB
YKB
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
Z�B
Z�B
[qB
[WB
[�B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
\�B
]/B
^B
^5B
^5B
^jB
^�B
^�B
_VB
_�B
_�B
_�B
`'B
`'B
`�B
aB
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
b�B
c B
b�B
cnB
c�B
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
e`B
e�B
f2B
f2B
ffB
ffB
gB
gB
gRB
gmB
g�B
g�B
h
B
h$B
h>B
h�B
h�B
h�B
i*B
iDB
i_B
i*B
iyB
jB
jKB
jKB
jeB
jeB
jKB
jeB
j�B
j�B
j�B
jeB
j0B
jKB
j0B
i�B
j0B
jKB
j�B
k�B
k6B
kQB
k�B
lB
l=B
lWB
l�B
mB
mCB
m]B
m]B
m]B
m]B
mwB
m�B
m�B
m�B
n�B
o B
o B
o B
oB
o B
n�B
n�B
n�B
o5B
oB
o5B
oOB
o�B
pB
pUB
p�B
p�B
p�B
qB
q'B
q�B
q�B
q�B
q�B
rB
q�B
rGB
r�B
r|B
r�B
r�B
r�B
sB
shB
s�B
s�B
tB
tB
t�B
t�B
uB
u%B
u?B
utB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v�B
v�B
v�B
v�B
wLB
wB
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
xRB
xlB
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191519  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191520                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041527  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041527  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                