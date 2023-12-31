CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:03Z creation;2022-06-04T19:17:04Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191703  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�kio1   @�kwwww@0^5?|��c��vȴ91   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�33B�33B�ffB�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Co�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @1�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B�(�B���B���B�B���B���B���B���B�(�B�(�B�\)B���B���B���B�B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C6{C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CV{CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cn{Co�GCq�GCs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN�DODO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\DǼ)D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�<)D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȀA��?A�ȴA��0A��A�ҽA���A�ӏA�� A��NA�бA��vA�ϫA��QA���A���A��A���A�0�Aȅ�AȎ�AȄMAȘ�A��tA��A��EA��/A���AȓuAȈ�A�9�A��A��yA���A��DAǹ�A�1[A��AŽA��,AçAµ�A� �A���A��}A� �A�j�A�/A�*0A�AA��2A���A�'A��A���A��_A�xA���A���A�t�A�T�A��A��A��A��iA���A��A��xA��*A�U�A�,=A���A�}�A�M�A��A�N�A��A�YA��jA���A���A��`A�JXA�tA���A���A�c A�0�A���A��A��+A�IA��RA�"�A~�A{9XAw&�At�\Ao+kAlb�Aie,Ah]dAg�	Af�5AfiDAe��Ad�|Ad(A^�AY�AV�9AV	AS�AQ��AO?�AK�AI��AF��AA��A?$tA:��A9A7��A6�A5D�A4N�A4/�A4;A3[�A1A�A.}VA-9�A,`�A+TaA'�A&�A$n�A"�3A!!A��A !�A ��A"3�A"��A#E9A#�}A#ȴA#�A"�PA"oiA"/�A!�TA!1'A �A˒A.�A��A>BA2aA.�A��A��A��A�rA'�A��AG�A�NA��A�A�A@AFtAPHAv�An�A�A�PAp;A)_AX�A*0A҉A��A9XA�A��A��A�zAGA�A�A�A��A	�_A�oA�WA.IAdZA6�AN�A�\A	AZ�A�AS�A{A��A�/A�A��A҉A��A��A ��A �'A [WA 
=@��@�@O@���@��m@��~@��n@�J#@�@@��@�ߤ@�@�ƨ@��B@홚@�r@�  @�|@�O�@�t@��.@��@�!�@�7�@�B[@�C-@�>B@�B[@��@���@�B[@��@�{J@�@O@���@�;�@�@��@���@�o@慈@�-@��@��T@�n@�L�@�?�@���@�O�@�~�@�N�@�B[@�1@ݵt@��@�@�@��Q@۲-@ۮ�@�{J@�8�@ښ@ٸ�@���@؞�@�kQ@�qv@�y>@��@Ձ�@�O@�G�@Ԡ�@�	�@�	�@�x@�b@��@��#@Ӂ@��K@�!@��>@ѩ�@��@Њr@��d@�Xy@͡�@�
=@��`@�D�@�c@�$@��@�8@�Q@��a@�t�@���@�ں@�ff@�>B@��}@�҉@�+k@ù�@Ò:@�o�@�Z�@�5�@��y@�@���@�S@���@�1@���@��@�x@�Dg@�C@��y@���@���@�l�@�+k@�u@���@�&�@��@�bN@�]d@�L0@��@��}@��@���@�.�@��^@�a@�Mj@��@���@���@�c@�:�@��"@��}@�`�@�E�@���@�@��C@�Vm@�|�@��@���@���@���@�S�@��v@���@�Xy@�>B@�)�@��o@�O�@��@���@��A@�J@��{@��@���@�_@���@�m]@�V@��D@��@�f�@���@���@��U@�/�@��@���@��	@��@���@�M�@�=q@�m�@�L0@�{@�}�@�33@��@���@�q@��@��@�o�@�͟@���@���@�ѷ@��\@�I�@�"h@��a@�qv@�O�@�-w@��X@�9X@���@�}�@�_p@�*0@��9@�Q@�  @��K@���@�j@�?}@�@��)@��@�B[@���@�A @��@��4@�H@�.�@��@��A@���@�X�@�#�@�Y@���@��R@���@�B[@��F@�H�@��@��x@���@��@�^5@�4@�u�@���@���@�%�@���@���@�\)@�"�@��@��R@�,=@��g@�$t@���@�A�@��@��D@�ԕ@���@���@�[W@�A�@��@��/@�Ĝ@���@���@��@���@�c@�9�@���@��H@��@�3�@���@�rG@�>�@�/@�,�@� i@��@��o@�<�@��@���@�Y�@���@��@��I@���@�e�@�:*@��D@��S@�Mj@���@���@���@�A�@��m@���@���@��@���@�A�@��@��_@�d�@�?@�6�@�.�@��3@�S�@�7L@�+@�֡@�Z�@��.@�ԕ@�}�@�T�@��|@��6@�v�@�L0@��@�a@�:@33@}�Z@}�^@}V@|?�@{��@{)_@{�@z{�@ze@y�3@yo @y&�@x�?@xz�@xbN@xD�@w�;@wS�@w�@v҉@v�@u}�@u&�@t�`@s��@so�@r�@r\�@rYK@r=q@q�^@pq@p?�@p-�@pM@o�Q@os@o@nz@m�@m%F@l��@lq@lbN@lV�@k�g@j�c@js�@j.�@i��@i��@i��@i:�@hM@g��@g�@f��@f�+@f��@fW�@fe@e�j@e��@e/@d��@d�@d7�@c��@c��@b��@b��@b3�@a@a�@aIR@a&�@`��@`�j@`��@`�@`V�@`~@_{J@_+@_�@^�@^�m@^�!@^��@^	@]��@]IR@]V@\�@\G@[�@@[@O@[+@Z��@Z��@Z�F@Z�A@ZR�@Z	@Y��@YY�@Y�@X_@X�@W�@WU�@V�M@V�6@Vxl@U�)@U^�@Uq@TM@S��@S�	@SP�@R�X@RW�@Re@Qԕ@Q��@QF@Q�@P�@O��@OZ�@O.I@N�c@N��@N� @Nn�@N5?@M��@Mc�@M0�@L�@Lg8@L/�@L'R@K��@K��@KC@J�y@J�X@J#:@I��@IB�@I�@H�|@H�v@H��@HV�@H4n@H  @Gƨ@G�	@GJ#@G,�@G�@F�@F��@FW�@FM�@FC�@E�@EJ�@D�@D�@C��@C�V@C33@C�@B�8@B�B@B��@A�@A/@@�O@@z�@@u�@@S�@@(�@?��@?��@?)_@>�]@>�@>��@>��@>W�@=�@=u�@<��@<��@<�I@<c�@<(�@;��@;�@;�a@;��@;��@;$t@:�}@:��@:B[@9�@9Y�@8�P@8�@87�@8M@8G@7��@6�8@6��@63�@5�@5��@5N<@4��@4�p@4��@4~(@4Z@4?�@4�@3�A@3�6@3��@39�@2�M@2��@2Ov@2?@1��@1�'@1�@1[W@1@@0�@0ی@0�O@0~(@0_@0M@0/�@0~@0�@/��@/a@/;d@/�@.ں@.��@.J�@.!�@-��@-hs@-V@,�@,ی@,�)@,�9@,[�@+�;@+��@+�:@+Z�@++@*�@*�}@*d�@*1�@*_@)ϫ@)��@)L�@(�[@(�O@(��@(	�@'��@'dZ@'33@&��@&��@&�b@&q�@%�Z@%��@%��@%�S@%p�@%&�@$�j@$oi@$H@$M@#خ@#�V@#X�@#+@"�8@"��@"�@"d�@"R�@"�@!�)@!��@!��@!IR@!;@ ��@ H@ �@�@��@'�@(@��@��@��@�F@xl@M�@!�@��@��@�@j@<6@*0@�@��@��@y>@]d@@�+@�@�@qv@;d@'�@��@�b@3�@
�@�j@��@J�@	l@��@tT@>B@��@iD@O@K�@�2@�x@M�@($@4@�@��@�C@��@J�@#�@��@�z@��@Z@K^@/�@@�;@��@��@y�@S�@�@�m@kQ@�@�3@�@/@�@��@�O@e�@1'@��@�W@�m@��@��@��@�f@o�@]�@C�@$t@Y@�@�@n�@YK@3�@	@�z@��@��@�'@��@��@w2@J�@V@�@��@I�@H@,=@�@��@��@�P@y�@Mj@33@ i@
��@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȀA��?A�ȴA��0A��A�ҽA���A�ӏA�� A��NA�бA��vA�ϫA��QA���A���A��A���A�0�Aȅ�AȎ�AȄMAȘ�A��tA��A��EA��/A���AȓuAȈ�A�9�A��A��yA���A��DAǹ�A�1[A��AŽA��,AçAµ�A� �A���A��}A� �A�j�A�/A�*0A�AA��2A���A�'A��A���A��_A�xA���A���A�t�A�T�A��A��A��A��iA���A��A��xA��*A�U�A�,=A���A�}�A�M�A��A�N�A��A�YA��jA���A���A��`A�JXA�tA���A���A�c A�0�A���A��A��+A�IA��RA�"�A~�A{9XAw&�At�\Ao+kAlb�Aie,Ah]dAg�	Af�5AfiDAe��Ad�|Ad(A^�AY�AV�9AV	AS�AQ��AO?�AK�AI��AF��AA��A?$tA:��A9A7��A6�A5D�A4N�A4/�A4;A3[�A1A�A.}VA-9�A,`�A+TaA'�A&�A$n�A"�3A!!A��A !�A ��A"3�A"��A#E9A#�}A#ȴA#�A"�PA"oiA"/�A!�TA!1'A �A˒A.�A��A>BA2aA.�A��A��A��A�rA'�A��AG�A�NA��A�A�A@AFtAPHAv�An�A�A�PAp;A)_AX�A*0A҉A��A9XA�A��A��A�zAGA�A�A�A��A	�_A�oA�WA.IAdZA6�AN�A�\A	AZ�A�AS�A{A��A�/A�A��A҉A��A��A ��A �'A [WA 
=@��@�@O@���@��m@��~@��n@�J#@�@@��@�ߤ@�@�ƨ@��B@홚@�r@�  @�|@�O�@�t@��.@��@�!�@�7�@�B[@�C-@�>B@�B[@��@���@�B[@��@�{J@�@O@���@�;�@�@��@���@�o@慈@�-@��@��T@�n@�L�@�?�@���@�O�@�~�@�N�@�B[@�1@ݵt@��@�@�@��Q@۲-@ۮ�@�{J@�8�@ښ@ٸ�@���@؞�@�kQ@�qv@�y>@��@Ձ�@�O@�G�@Ԡ�@�	�@�	�@�x@�b@��@��#@Ӂ@��K@�!@��>@ѩ�@��@Њr@��d@�Xy@͡�@�
=@��`@�D�@�c@�$@��@�8@�Q@��a@�t�@���@�ں@�ff@�>B@��}@�҉@�+k@ù�@Ò:@�o�@�Z�@�5�@��y@�@���@�S@���@�1@���@��@�x@�Dg@�C@��y@���@���@�l�@�+k@�u@���@�&�@��@�bN@�]d@�L0@��@��}@��@���@�.�@��^@�a@�Mj@��@���@���@�c@�:�@��"@��}@�`�@�E�@���@�@��C@�Vm@�|�@��@���@���@���@�S�@��v@���@�Xy@�>B@�)�@��o@�O�@��@���@��A@�J@��{@��@���@�_@���@�m]@�V@��D@��@�f�@���@���@��U@�/�@��@���@��	@��@���@�M�@�=q@�m�@�L0@�{@�}�@�33@��@���@�q@��@��@�o�@�͟@���@���@�ѷ@��\@�I�@�"h@��a@�qv@�O�@�-w@��X@�9X@���@�}�@�_p@�*0@��9@�Q@�  @��K@���@�j@�?}@�@��)@��@�B[@���@�A @��@��4@�H@�.�@��@��A@���@�X�@�#�@�Y@���@��R@���@�B[@��F@�H�@��@��x@���@��@�^5@�4@�u�@���@���@�%�@���@���@�\)@�"�@��@��R@�,=@��g@�$t@���@�A�@��@��D@�ԕ@���@���@�[W@�A�@��@��/@�Ĝ@���@���@��@���@�c@�9�@���@��H@��@�3�@���@�rG@�>�@�/@�,�@� i@��@��o@�<�@��@���@�Y�@���@��@��I@���@�e�@�:*@��D@��S@�Mj@���@���@���@�A�@��m@���@���@��@���@�A�@��@��_@�d�@�?@�6�@�.�@��3@�S�@�7L@�+@�֡@�Z�@��.@�ԕ@�}�@�T�@��|@��6@�v�@�L0@��@�a@�:@33@}�Z@}�^@}V@|?�@{��@{)_@{�@z{�@ze@y�3@yo @y&�@x�?@xz�@xbN@xD�@w�;@wS�@w�@v҉@v�@u}�@u&�@t�`@s��@so�@r�@r\�@rYK@r=q@q�^@pq@p?�@p-�@pM@o�Q@os@o@nz@m�@m%F@l��@lq@lbN@lV�@k�g@j�c@js�@j.�@i��@i��@i��@i:�@hM@g��@g�@f��@f�+@f��@fW�@fe@e�j@e��@e/@d��@d�@d7�@c��@c��@b��@b��@b3�@a@a�@aIR@a&�@`��@`�j@`��@`�@`V�@`~@_{J@_+@_�@^�@^�m@^�!@^��@^	@]��@]IR@]V@\�@\G@[�@@[@O@[+@Z��@Z��@Z�F@Z�A@ZR�@Z	@Y��@YY�@Y�@X_@X�@W�@WU�@V�M@V�6@Vxl@U�)@U^�@Uq@TM@S��@S�	@SP�@R�X@RW�@Re@Qԕ@Q��@QF@Q�@P�@O��@OZ�@O.I@N�c@N��@N� @Nn�@N5?@M��@Mc�@M0�@L�@Lg8@L/�@L'R@K��@K��@KC@J�y@J�X@J#:@I��@IB�@I�@H�|@H�v@H��@HV�@H4n@H  @Gƨ@G�	@GJ#@G,�@G�@F�@F��@FW�@FM�@FC�@E�@EJ�@D�@D�@C��@C�V@C33@C�@B�8@B�B@B��@A�@A/@@�O@@z�@@u�@@S�@@(�@?��@?��@?)_@>�]@>�@>��@>��@>W�@=�@=u�@<��@<��@<�I@<c�@<(�@;��@;�@;�a@;��@;��@;$t@:�}@:��@:B[@9�@9Y�@8�P@8�@87�@8M@8G@7��@6�8@6��@63�@5�@5��@5N<@4��@4�p@4��@4~(@4Z@4?�@4�@3�A@3�6@3��@39�@2�M@2��@2Ov@2?@1��@1�'@1�@1[W@1@@0�@0ی@0�O@0~(@0_@0M@0/�@0~@0�@/��@/a@/;d@/�@.ں@.��@.J�@.!�@-��@-hs@-V@,�@,ی@,�)@,�9@,[�@+�;@+��@+�:@+Z�@++@*�@*�}@*d�@*1�@*_@)ϫ@)��@)L�@(�[@(�O@(��@(	�@'��@'dZ@'33@&��@&��@&�b@&q�@%�Z@%��@%��@%�S@%p�@%&�@$�j@$oi@$H@$M@#خ@#�V@#X�@#+@"�8@"��@"�@"d�@"R�@"�@!�)@!��@!��@!IR@!;@ ��@ H@ �@�@��@'�@(@��@��@��@�F@xl@M�@!�@��@��@�@j@<6@*0@�@��@��@y>@]d@@�+@�@�@qv@;d@'�@��@�b@3�@
�@�j@��@J�@	l@��@tT@>B@��@iD@O@K�@�2@�x@M�@($@4@�@��@�C@��@J�@#�@��@�z@��@Z@K^@/�@@�;@��@��@y�@S�@�@�m@kQ@�@�3@�@/@�@��@�O@e�@1'@��@�W@�m@��@��@��@�f@o�@]�@C�@$t@Y@�@�@n�@YK@3�@	@�z@��@��@�'@��@��@w2@J�@V@�@��@I�@H@,=@�@��@��@�P@y�@Mj@33@ i@
��@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�+B	�+B	��B	��B	��B	��B	��B	�B	�+B	�B	�EB	�zB	�B	��B	��B	��B	�,B	�B	��B	�vB
�B
�B
1�B
9>B
B�B
Q�B
c�B
vFB
v�B
rGB
r�B
|jB
�'B
�\B
��B
��B
�[B
��B
�BUB�BB0ByrB�RB�B�,B�B��B��B��B��B��B��B�oB޸B��B�B�,B�B�&B߾B�yB�vB�B�}B̘B�6B��B��B�2BT�B>�B;dB1�B#B�BSB�B
�B
�B
��B
��B
��B
�lB
��B
��B
�B
kB
QB
<�B
2aB
(XB
_B	��B	�tB	уB	��B	��B	�EB	~wB	{B	u�B	qB	l�B	f�B	^�B	E�B	,"B	OB	�B	�B	DB��B��BޞB�,B��B�FB�B��B��B�B�bB�'B�BB�B��B�B��B��B��B�dB�B�B��B��B�FB�B�BٚB�(B	QB	'B	=�B	C�B	K�B	O�B	Q4B	W?B	dZB	j�B	l�B	oOB	r�B	t9B	v+B	xlB	}VB	�B	�B	��B	� B	}�B	y	B	wfB	y	B	}VB	�B	��B	�B	�!B	��B	�B	��B	��B	�B	��B	�B	��B	�?B	��B	ȚB	�)B	��B	��B	��B	�`B	��B	��B	��B	�B	��B	��B	iB	f2B	iyB	e`B	d�B	g8B	mB	x�B	�YB	�B	��B	�4B	��B	��B	��B	��B	��B	�VB	��B	�B	��B	�>B	��B	�B	��B	�wB	��B	�mB	�jB	��B	�]B	�QB	�,B	�7B	}�B	z�B	zDB	|6B	|�B	.B	��B	��B	��B	�B	��B	��B	�aB	�MB	��B	�B	�B	��B	�^B	��B	��B	�;B	��B	�'B	�oB	�}B	�BB	��B	�VB	��B	�"B	�VB	�<B	��B	��B	�;B	ňB	�lB	�B	�DB	�dB	��B	͟B	�\B	��B	��B	��B	�HB	�}B	�hB	҉B	�TB	�:B	��B	�TB	�B	ѷB	� B	�oB	� B	ӏB	�{B	�{B	�{B	�{B	�{B	ԯB	յB	�SB	׍B	��B	��B	�KB	�B	�B	�WB	�	B	�WB	یB	��B	خB	�B	�MB	ٚB	�BB	�B	�HB	��B	��B	�NB	�4B	�B	�B	�&B	�B	��B	��B	�B	�B	�`B	�B	�LB	�B	�B	�B	�8B	�B	�
B	�B	��B	��B	��B	�kB	�B	�CB	�B	��B	�B	�]B	��B	��B	�B	�WB	��B	�B	�B	�qB	��B	�B	�B	�/B	�B	�B	�UB	�;B	��B	�'B	�B	��B	�aB	�B	��B	�-B	�hB	�B	�9B	�B	�9B	�9B	�B	�%B	��B	�nB	�9B	�9B	��B	��B	��B	��B	�tB	��B	��B	�LB	��B	�RB	�lB	��B	�XB	�rB	��B	�xB	��B	��B	�"B	��B	�jB	�PB	�B	�VB	�qB	��B	��B
 �B
UB
B
 4B	��B	�}B	��B
  B
�B
�B
;B
UB
�B
GB
aB
�B
�B
�B
�B
%B
%B
%B
?B
�B
�B
�B
�B
1B
�B
1B
fB
�B
	B
�B
�B
	B
	B
	7B
	7B

�B

�B
B
�B
xB
^B
�B
�B
~B
�B
�B
6B
�B
�B
�B
"B
�B
\B
�B
�B
 B
�B
HB
B
B
B
�B
B
�B
aB
B
�B
�B
�B
�B
�B
_B
+B
�B
7B
7B
B
B
�B
�B
]B
/B
�B
�B
~B
�B
B
�B
dB
�B
�B
B
B
jB
�B
�B
�B
�B
�B
!B
VB
�B
�B
!B
�B
�B
VB
 BB
!B
 �B
!-B
!�B
!bB
!B
 �B
 �B
!B
"4B
"4B
"4B
"NB
"4B
"�B
#nB
#nB
$ZB
$�B
%FB
%,B
%B
%�B
%�B
%�B
&LB
'mB
(�B
(�B
)B
)�B
)�B
*B
*�B
*�B
+6B
+QB
+�B
+�B
+�B
-�B
-�B
.�B
.�B
/�B
/�B
0UB
1�B
2-B
2GB
2|B
2|B
2GB
2B
1�B
1�B
1�B
2B
1�B
1�B
2aB
2|B
2�B
2�B
3�B
3�B
4�B
4�B
4�B
4nB
5B
6+B
6B
6+B
6B
6FB
6zB
6�B
6�B
7�B
8B
8RB
9	B
9XB
9XB
9�B
:DB
9�B
:B
9�B
9�B
9�B
:DB
;B
;�B
<PB
<�B
<�B
<�B
<�B
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@4B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A�B
B'B
BB
B'B
BAB
B[B
BAB
B�B
B�B
B�B
B�B
CB
C�B
DMB
D�B
D�B
D�B
EB
E9B
ESB
EmB
EmB
F%B
FYB
F�B
GEB
G�B
G�B
G�B
HB
H1B
HKB
H�B
IB
IB
I�B
I�B
J	B
J	B
J�B
KB
K)B
KDB
K�B
K�B
LB
LB
L�B
L�B
L�B
L�B
L�B
MB
MB
M6B
M�B
M�B
N"B
NB
N"B
N<B
N�B
N�B
OBB
O�B
O�B
OvB
PB
P}B
P�B
P�B
P�B
P�B
Q B
QB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RTB
RoB
RoB
R:B
R�B
R�B
SuB
S�B
S�B
S�B
TB
TB
TB
S�B
S�B
TaB
T�B
UB
UMB
UMB
UMB
UgB
U�B
U�B
U�B
VB
V9B
V9B
V9B
V�B
V�B
WYB
WsB
WsB
WsB
W�B
W�B
XEB
X_B
X_B
XyB
X_B
X�B
X�B
X�B
YB
YKB
Y�B
Y�B
ZB
Z7B
ZQB
ZB
ZQB
Z�B
[	B
[qB
[�B
[�B
\B
\]B
\CB
\xB
\xB
\�B
\xB
\�B
\�B
\�B
]B
]/B
]dB
]�B
]�B
]�B
]�B
^B
^B
^B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_pB
_�B
_�B
_�B
`B
`'B
`�B
`�B
aHB
a-B
aHB
aHB
aHB
a|B
a�B
bB
bNB
b�B
b�B
b�B
b�B
cB
c B
c:B
cTB
cnB
c�B
d&B
d&B
d@B
d�B
d�B
eB
eFB
e`B
ezB
e�B
e�B
ffB
fLB
f�B
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
i*B
iDB
i_B
iyB
i�B
i�B
jeB
j�B
j�B
j�B
k6B
k�B
k�B
k�B
k�B
l=B
lB
l"B
lWB
lWB
l�B
l�B
l�B
m)B
mCB
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
nIB
ncB
n�B
o B
o B
o5B
oOB
o�B
o�B
o�B
p;B
pUB
p�B
p�B
q'B
qAB
q�B
rB
rB
q�B
r|B
r�B
r�B
sB
sB
sMB
shB
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
wLB
w�B
w�B
x8B
xRB
xlB
xRB
x�B
x�B
y$B
y	B
y	B
y$B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
z^B
zxB
zxB
z�B
z�B
{0B
{B
{0B
{0B
{0B
{JB
{JB
{�B
{�B
|B
|6B
|�B
|jB
|�B
|�B
}"B
}"B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�+B	�+B	��B	��B	��B	��B	��B	�B	�+B	�B	�EB	�zB	�B	��B	��B	��B	�,B	�B	��B	�vB
�B
�B
1�B
9>B
B�B
Q�B
c�B
vFB
v�B
rGB
r�B
|jB
�'B
�\B
��B
��B
�[B
��B
�BUB�BB0ByrB�RB�B�,B�B��B��B��B��B��B��B�oB޸B��B�B�,B�B�&B߾B�yB�vB�B�}B̘B�6B��B��B�2BT�B>�B;dB1�B#B�BSB�B
�B
�B
��B
��B
��B
�lB
��B
��B
�B
kB
QB
<�B
2aB
(XB
_B	��B	�tB	уB	��B	��B	�EB	~wB	{B	u�B	qB	l�B	f�B	^�B	E�B	,"B	OB	�B	�B	DB��B��BޞB�,B��B�FB�B��B��B�B�bB�'B�BB�B��B�B��B��B��B�dB�B�B��B��B�FB�B�BٚB�(B	QB	'B	=�B	C�B	K�B	O�B	Q4B	W?B	dZB	j�B	l�B	oOB	r�B	t9B	v+B	xlB	}VB	�B	�B	��B	� B	}�B	y	B	wfB	y	B	}VB	�B	��B	�B	�!B	��B	�B	��B	��B	�B	��B	�B	��B	�?B	��B	ȚB	�)B	��B	��B	��B	�`B	��B	��B	��B	�B	��B	��B	iB	f2B	iyB	e`B	d�B	g8B	mB	x�B	�YB	�B	��B	�4B	��B	��B	��B	��B	��B	�VB	��B	�B	��B	�>B	��B	�B	��B	�wB	��B	�mB	�jB	��B	�]B	�QB	�,B	�7B	}�B	z�B	zDB	|6B	|�B	.B	��B	��B	��B	�B	��B	��B	�aB	�MB	��B	�B	�B	��B	�^B	��B	��B	�;B	��B	�'B	�oB	�}B	�BB	��B	�VB	��B	�"B	�VB	�<B	��B	��B	�;B	ňB	�lB	�B	�DB	�dB	��B	͟B	�\B	��B	��B	��B	�HB	�}B	�hB	҉B	�TB	�:B	��B	�TB	�B	ѷB	� B	�oB	� B	ӏB	�{B	�{B	�{B	�{B	�{B	ԯB	յB	�SB	׍B	��B	��B	�KB	�B	�B	�WB	�	B	�WB	یB	��B	خB	�B	�MB	ٚB	�BB	�B	�HB	��B	��B	�NB	�4B	�B	�B	�&B	�B	��B	��B	�B	�B	�`B	�B	�LB	�B	�B	�B	�8B	�B	�
B	�B	��B	��B	��B	�kB	�B	�CB	�B	��B	�B	�]B	��B	��B	�B	�WB	��B	�B	�B	�qB	��B	�B	�B	�/B	�B	�B	�UB	�;B	��B	�'B	�B	��B	�aB	�B	��B	�-B	�hB	�B	�9B	�B	�9B	�9B	�B	�%B	��B	�nB	�9B	�9B	��B	��B	��B	��B	�tB	��B	��B	�LB	��B	�RB	�lB	��B	�XB	�rB	��B	�xB	��B	��B	�"B	��B	�jB	�PB	�B	�VB	�qB	��B	��B
 �B
UB
B
 4B	��B	�}B	��B
  B
�B
�B
;B
UB
�B
GB
aB
�B
�B
�B
�B
%B
%B
%B
?B
�B
�B
�B
�B
1B
�B
1B
fB
�B
	B
�B
�B
	B
	B
	7B
	7B

�B

�B
B
�B
xB
^B
�B
�B
~B
�B
�B
6B
�B
�B
�B
"B
�B
\B
�B
�B
 B
�B
HB
B
B
B
�B
B
�B
aB
B
�B
�B
�B
�B
�B
_B
+B
�B
7B
7B
B
B
�B
�B
]B
/B
�B
�B
~B
�B
B
�B
dB
�B
�B
B
B
jB
�B
�B
�B
�B
�B
!B
VB
�B
�B
!B
�B
�B
VB
 BB
!B
 �B
!-B
!�B
!bB
!B
 �B
 �B
!B
"4B
"4B
"4B
"NB
"4B
"�B
#nB
#nB
$ZB
$�B
%FB
%,B
%B
%�B
%�B
%�B
&LB
'mB
(�B
(�B
)B
)�B
)�B
*B
*�B
*�B
+6B
+QB
+�B
+�B
+�B
-�B
-�B
.�B
.�B
/�B
/�B
0UB
1�B
2-B
2GB
2|B
2|B
2GB
2B
1�B
1�B
1�B
2B
1�B
1�B
2aB
2|B
2�B
2�B
3�B
3�B
4�B
4�B
4�B
4nB
5B
6+B
6B
6+B
6B
6FB
6zB
6�B
6�B
7�B
8B
8RB
9	B
9XB
9XB
9�B
:DB
9�B
:B
9�B
9�B
9�B
:DB
;B
;�B
<PB
<�B
<�B
<�B
<�B
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@4B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A�B
B'B
BB
B'B
BAB
B[B
BAB
B�B
B�B
B�B
B�B
CB
C�B
DMB
D�B
D�B
D�B
EB
E9B
ESB
EmB
EmB
F%B
FYB
F�B
GEB
G�B
G�B
G�B
HB
H1B
HKB
H�B
IB
IB
I�B
I�B
J	B
J	B
J�B
KB
K)B
KDB
K�B
K�B
LB
LB
L�B
L�B
L�B
L�B
L�B
MB
MB
M6B
M�B
M�B
N"B
NB
N"B
N<B
N�B
N�B
OBB
O�B
O�B
OvB
PB
P}B
P�B
P�B
P�B
P�B
Q B
QB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RTB
RoB
RoB
R:B
R�B
R�B
SuB
S�B
S�B
S�B
TB
TB
TB
S�B
S�B
TaB
T�B
UB
UMB
UMB
UMB
UgB
U�B
U�B
U�B
VB
V9B
V9B
V9B
V�B
V�B
WYB
WsB
WsB
WsB
W�B
W�B
XEB
X_B
X_B
XyB
X_B
X�B
X�B
X�B
YB
YKB
Y�B
Y�B
ZB
Z7B
ZQB
ZB
ZQB
Z�B
[	B
[qB
[�B
[�B
\B
\]B
\CB
\xB
\xB
\�B
\xB
\�B
\�B
\�B
]B
]/B
]dB
]�B
]�B
]�B
]�B
^B
^B
^B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_pB
_�B
_�B
_�B
`B
`'B
`�B
`�B
aHB
a-B
aHB
aHB
aHB
a|B
a�B
bB
bNB
b�B
b�B
b�B
b�B
cB
c B
c:B
cTB
cnB
c�B
d&B
d&B
d@B
d�B
d�B
eB
eFB
e`B
ezB
e�B
e�B
ffB
fLB
f�B
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
i*B
iDB
i_B
iyB
i�B
i�B
jeB
j�B
j�B
j�B
k6B
k�B
k�B
k�B
k�B
l=B
lB
l"B
lWB
lWB
l�B
l�B
l�B
m)B
mCB
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
nIB
ncB
n�B
o B
o B
o5B
oOB
o�B
o�B
o�B
p;B
pUB
p�B
p�B
q'B
qAB
q�B
rB
rB
q�B
r|B
r�B
r�B
sB
sB
sMB
shB
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
wLB
w�B
w�B
x8B
xRB
xlB
xRB
x�B
x�B
y$B
y	B
y	B
y$B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
z^B
zxB
zxB
z�B
z�B
{0B
{B
{0B
{0B
{0B
{JB
{JB
{�B
{�B
|B
|6B
|�B
|jB
|�B
|�B
}"B
}"B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191703  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191704  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191704                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041712  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041712  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                