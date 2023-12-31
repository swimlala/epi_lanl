CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:21:21Z creation;2022-06-04T19:21:21Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192121  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�>����	1   @�>�?V�@-��S���ci�hr�!1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�ffB�  B�  B�  B���B�33B���B�33B���B���B���B�  B�ffBϙ�B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C � C!��C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP�CR�CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @+�@~�R@�\)@�\)AzA?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�BQ�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B�(�B���B�\)B���B���B���B��]B�(�B�B�(�B�B�B�B���B�\)BϏ]B���B���B���B���B���B���B���B���B�(�B�(�B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C z�C!ǮC#�GC%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE�GCG��CI��CK��CM��CP{CR{CS��CU��CW��CY��C[�GC]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�GC{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQxRDQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A�خA���A��UAۺ*Aۘ_A�r�A�l"A�j�A�f�A�`vA�QNA�G�A�B�A�=�A�;dA�8�A�6�A�5tA�2�A�/�A�+kA�*�A�&�A�"�A� 'A��A��A�ȀA�?�A�!�A�i�Aԑ4A�2aA��A���Aʂ�A�� A��7A�-wA��3A�%A�N�A�?�A�gmA���A��MA���A��A��A���A�1[A�͟A���A��xA��8A��vA��A�A�YA���A��A���A�4A��A��A��+A��7A�+A��~A��A�ҽA���A�iyA��AB[Azs�Aw�DAm�Al&Aj�Ah7Adi�Aa��A^�1AZ��AT��AS�AQAOGEANAJk�AG��AF_AD'RAA��A?F�A<e�A;��A:�A9J#A8zA8E9A7��A6�A3cA0zxA,��A)f�A'�[A(|�A(��A'ݘA&�tA%��A%RTA$�HA#�]A"�MA"��A!�A!��A ��A�Aa|A	lA�eA�A��A��A6A!-A�MAOA��A��A>BA�A��A��A,=A�}A�{A;dA<6A�jA��A@OA��A�A��A>�A�A�'A�.Ay�A�\A�TA� A�A�An�A��AYA�|A�IAH�A�A��A
��A	�Aq�A`BA��A�A�A��A
=A>�AjA��AT�A �A@�7�@�U2@�7@��@���@�H�@���@�B�@�M�@�.@��@��"@�q@��@��@��@��@�!�@���@��@�f@�c�@�@O@��@�_@�	@�C�@�7�@�8�@�&�@��@��'@�S�@�hs@�!@�<�@�8�@���@��@�@�YK@�7L@�;@��@�GE@���@ؾ@��@�\�@ٍP@چY@�_@��@��@߃{@�o�@��@�2a@�s�@�+@މ�@���@��@��@�m�@۞�@��@�?�@��@�bN@��m@��E@�A�@��@��@�s@ԗ�@ӫ�@���@үO@Ҁ�@�($@��;@�x@��@�`�@�9�@ά@��@��r@͉7@�Y@�*�@�t�@���@� �@�a@ȔF@�B[@��@ǎ"@�G�@��,@�ݘ@�%F@��f@��)@�y>@��]@ÄM@���@�@�3�@���@�&�@�֡@��b@�H�@�  @��@�7L@��,@�xl@�?�@��@��4@�9�@��"@��v@���@���@���@�;d@�<6@��@��E@�~�@���@�,�@���@��]@���@��+@��@��@��\@�!@���@���@��@�;�@�"�@���@���@�Ov@��@���@���@�V@���@��}@�{@���@��C@�X�@�q@�q@��@���@��@��6@���@�W?@��y@��r@�_@��@���@�x�@��@��4@�q@�H@�@��4@�F@��@���@�V�@��@��d@��P@�|�@�s�@�8�@��X@���@�Z�@�9X@� �@��
@��'@�:�@���@��@���@��Q@���@�Dg@�V@��@�B[@�s�@��@��$@���@�:�@���@��@�S&@��K@�w�@�C�@�@��@��q@��4@�7L@��@��4@�($@��@��T@��H@��@�N<@���@��I@�u@��:@�RT@�-w@��@��j@��.@�e@���@���@�j�@�'�@��@��H@��\@�u�@�Q�@�e@��@��@�]�@� \@���@��o@�7�@��A@�}�@�@�ѷ@��}@���@�z�@�U2@���@�s@�K�@��@���@�J�@�@��6@�x@�+�@��@��R@��@�ff@��@���@���@��@��@�h�@�tT@�i�@�O@��r@��@��@@�S�@�@@���@�xl@�D�@�%�@�� @�Z�@��@���@�^5@�	@���@���@�RT@�F�@�9�@�-w@��@��@��f@���@���@�YK@�7�@��@��@��[@�o @�W?@�J�@�/@�o@���@��c@��,@���@�tT@�2�@��;@�Q�@�V@��@���@�?@���@��K@��~@�T�@�8@��@��L@�e�@�~@�}@x@'�@~��@~�8@~�!@~($@}��@}��@}��@}q@|�@|��@|��@|<�@|�@{�@{�f@z�H@zl�@y�@y�@y��@y\�@y�@x��@xXy@xx@w��@v�@v{�@vM�@v{@u�^@u<6@t��@tN�@s��@s��@sg�@s&@r��@r�R@r+k@q��@q-w@p�E@ph�@o�F@o��@oP�@oC@n�!@n#:@mw2@l�@lu�@l?�@k�+@kU�@j�@jkQ@j$�@i�@i��@i�@iw2@i�@h��@h��@hh�@h6@g��@g�@@gn/@g�@f�@f��@f�@f)�@e�@e4@e�@d�@dѷ@d�Y@cخ@cy�@cS�@b��@b��@b�6@b�r@bTa@b	@a�N@a�"@a4@`�@_�+@_�@^�+@^#:@]��@]�h@]k�@]+�@\��@\r�@\ �@[�Q@[�w@[o�@[6z@Z��@Z��@Zd�@Y��@Yk�@YB�@Y%@X��@XV�@W�*@W�@V��@VOv@V$�@U�@U��@U7L@T�u@TQ�@S�@SA�@R�h@ROv@Q��@Q�@Qzx@Q�@P�@P�@P�@P4n@O��@O�P@O!-@N҉@N��@N�@Nu%@Nc @M}�@L�|@L�p@LɆ@LXy@K��@K��@Ke�@K�@J��@Je@I��@I��@H�4@G�r@G�[@G�@G\)@G'�@G�@F҉@F�@F�\@Fd�@F-@E�#@E��@EIR@Dی@Dm�@DK^@Dx@C��@C��@C��@C��@CRT@C@B��@BB[@B�@A��@A`B@A	l@@��@@��@@r�@@M@?�*@>��@>�!@>�r@>YK@>	@=��@=�@<�@<e�@<b@;F�@:�,@:{�@:e@9o @9;@8ѷ@8]d@7��@7g�@7�@6�B@6�A@6:*@5��@5�@5p�@5+@4�@47�@4b@3��@3�@2��@25?@1��@1�@1�@1�o@1��@1F@0��@0��@0PH@0~@/��@/F�@.�2@.��@.xl@.	@-�T@-�^@-��@-s�@-2a@,�z@,tT@+ݘ@+�@+l�@+.I@*�y@*{�@)�@)c@)-w@(�P@(��@(�@(Z@(6@'��@'�{@&�@&��@&��@&a|@%��@%2a@%;@$��@$y>@$`�@#خ@#�:@#��@#{J@#E9@"�M@"� @"C�@!��@!�^@!�@![W@!G�@!+�@ ��@ ��@ ��@ ?�@ 4n@ b@��@��@P�@��@��@v�@YK@-@ �@��@Y�@�@�@�?@g8@�@�[@dZ@9�@�@�@�6@�F@W�@�@��@�@�j@��@rG@G�@0�@(�@�@�@�@��@>B@�A@�k@P�@F�@/�@�@�R@?@3�@!�@�)@��@��@�"@�h@u�@a�@Dg@�@��@[�@6@~@��@�6@��@RT@�@�@��@xl@h
@($@�T@��@�M@rG@G�@��@��@�Y@Q�@7�@�@��@��@��@��@|�@o�@C@�y@��@�\@Ta@��@�z@�C@��@f�@�@�$@l"@�@�@�K@��@'�@�@
��@
�!@
��@
n�@
3�@	�o@	�@	��@	}�@	T�@	�@��@��@w�@>B@�@�@��@=@o@S@�@�@ߤ@ں@҉@ȴ@�R@��@ff@B[@�D@�N@��@�@��@��@��@�@}�@o @T�@-w@�@��@��@��@�e@��@��@q@]d@M@Ft@<�@�@x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A�خA���A��UAۺ*Aۘ_A�r�A�l"A�j�A�f�A�`vA�QNA�G�A�B�A�=�A�;dA�8�A�6�A�5tA�2�A�/�A�+kA�*�A�&�A�"�A� 'A��A��A�ȀA�?�A�!�A�i�Aԑ4A�2aA��A���Aʂ�A�� A��7A�-wA��3A�%A�N�A�?�A�gmA���A��MA���A��A��A���A�1[A�͟A���A��xA��8A��vA��A�A�YA���A��A���A�4A��A��A��+A��7A�+A��~A��A�ҽA���A�iyA��AB[Azs�Aw�DAm�Al&Aj�Ah7Adi�Aa��A^�1AZ��AT��AS�AQAOGEANAJk�AG��AF_AD'RAA��A?F�A<e�A;��A:�A9J#A8zA8E9A7��A6�A3cA0zxA,��A)f�A'�[A(|�A(��A'ݘA&�tA%��A%RTA$�HA#�]A"�MA"��A!�A!��A ��A�Aa|A	lA�eA�A��A��A6A!-A�MAOA��A��A>BA�A��A��A,=A�}A�{A;dA<6A�jA��A@OA��A�A��A>�A�A�'A�.Ay�A�\A�TA� A�A�An�A��AYA�|A�IAH�A�A��A
��A	�Aq�A`BA��A�A�A��A
=A>�AjA��AT�A �A@�7�@�U2@�7@��@���@�H�@���@�B�@�M�@�.@��@��"@�q@��@��@��@��@�!�@���@��@�f@�c�@�@O@��@�_@�	@�C�@�7�@�8�@�&�@��@��'@�S�@�hs@�!@�<�@�8�@���@��@�@�YK@�7L@�;@��@�GE@���@ؾ@��@�\�@ٍP@چY@�_@��@��@߃{@�o�@��@�2a@�s�@�+@މ�@���@��@��@�m�@۞�@��@�?�@��@�bN@��m@��E@�A�@��@��@�s@ԗ�@ӫ�@���@үO@Ҁ�@�($@��;@�x@��@�`�@�9�@ά@��@��r@͉7@�Y@�*�@�t�@���@� �@�a@ȔF@�B[@��@ǎ"@�G�@��,@�ݘ@�%F@��f@��)@�y>@��]@ÄM@���@�@�3�@���@�&�@�֡@��b@�H�@�  @��@�7L@��,@�xl@�?�@��@��4@�9�@��"@��v@���@���@���@�;d@�<6@��@��E@�~�@���@�,�@���@��]@���@��+@��@��@��\@�!@���@���@��@�;�@�"�@���@���@�Ov@��@���@���@�V@���@��}@�{@���@��C@�X�@�q@�q@��@���@��@��6@���@�W?@��y@��r@�_@��@���@�x�@��@��4@�q@�H@�@��4@�F@��@���@�V�@��@��d@��P@�|�@�s�@�8�@��X@���@�Z�@�9X@� �@��
@��'@�:�@���@��@���@��Q@���@�Dg@�V@��@�B[@�s�@��@��$@���@�:�@���@��@�S&@��K@�w�@�C�@�@��@��q@��4@�7L@��@��4@�($@��@��T@��H@��@�N<@���@��I@�u@��:@�RT@�-w@��@��j@��.@�e@���@���@�j�@�'�@��@��H@��\@�u�@�Q�@�e@��@��@�]�@� \@���@��o@�7�@��A@�}�@�@�ѷ@��}@���@�z�@�U2@���@�s@�K�@��@���@�J�@�@��6@�x@�+�@��@��R@��@�ff@��@���@���@��@��@�h�@�tT@�i�@�O@��r@��@��@@�S�@�@@���@�xl@�D�@�%�@�� @�Z�@��@���@�^5@�	@���@���@�RT@�F�@�9�@�-w@��@��@��f@���@���@�YK@�7�@��@��@��[@�o @�W?@�J�@�/@�o@���@��c@��,@���@�tT@�2�@��;@�Q�@�V@��@���@�?@���@��K@��~@�T�@�8@��@��L@�e�@�~@�}@x@'�@~��@~�8@~�!@~($@}��@}��@}��@}q@|�@|��@|��@|<�@|�@{�@{�f@z�H@zl�@y�@y�@y��@y\�@y�@x��@xXy@xx@w��@v�@v{�@vM�@v{@u�^@u<6@t��@tN�@s��@s��@sg�@s&@r��@r�R@r+k@q��@q-w@p�E@ph�@o�F@o��@oP�@oC@n�!@n#:@mw2@l�@lu�@l?�@k�+@kU�@j�@jkQ@j$�@i�@i��@i�@iw2@i�@h��@h��@hh�@h6@g��@g�@@gn/@g�@f�@f��@f�@f)�@e�@e4@e�@d�@dѷ@d�Y@cخ@cy�@cS�@b��@b��@b�6@b�r@bTa@b	@a�N@a�"@a4@`�@_�+@_�@^�+@^#:@]��@]�h@]k�@]+�@\��@\r�@\ �@[�Q@[�w@[o�@[6z@Z��@Z��@Zd�@Y��@Yk�@YB�@Y%@X��@XV�@W�*@W�@V��@VOv@V$�@U�@U��@U7L@T�u@TQ�@S�@SA�@R�h@ROv@Q��@Q�@Qzx@Q�@P�@P�@P�@P4n@O��@O�P@O!-@N҉@N��@N�@Nu%@Nc @M}�@L�|@L�p@LɆ@LXy@K��@K��@Ke�@K�@J��@Je@I��@I��@H�4@G�r@G�[@G�@G\)@G'�@G�@F҉@F�@F�\@Fd�@F-@E�#@E��@EIR@Dی@Dm�@DK^@Dx@C��@C��@C��@C��@CRT@C@B��@BB[@B�@A��@A`B@A	l@@��@@��@@r�@@M@?�*@>��@>�!@>�r@>YK@>	@=��@=�@<�@<e�@<b@;F�@:�,@:{�@:e@9o @9;@8ѷ@8]d@7��@7g�@7�@6�B@6�A@6:*@5��@5�@5p�@5+@4�@47�@4b@3��@3�@2��@25?@1��@1�@1�@1�o@1��@1F@0��@0��@0PH@0~@/��@/F�@.�2@.��@.xl@.	@-�T@-�^@-��@-s�@-2a@,�z@,tT@+ݘ@+�@+l�@+.I@*�y@*{�@)�@)c@)-w@(�P@(��@(�@(Z@(6@'��@'�{@&�@&��@&��@&a|@%��@%2a@%;@$��@$y>@$`�@#خ@#�:@#��@#{J@#E9@"�M@"� @"C�@!��@!�^@!�@![W@!G�@!+�@ ��@ ��@ ��@ ?�@ 4n@ b@��@��@P�@��@��@v�@YK@-@ �@��@Y�@�@�@�?@g8@�@�[@dZ@9�@�@�@�6@�F@W�@�@��@�@�j@��@rG@G�@0�@(�@�@�@�@��@>B@�A@�k@P�@F�@/�@�@�R@?@3�@!�@�)@��@��@�"@�h@u�@a�@Dg@�@��@[�@6@~@��@�6@��@RT@�@�@��@xl@h
@($@�T@��@�M@rG@G�@��@��@�Y@Q�@7�@�@��@��@��@��@|�@o�@C@�y@��@�\@Ta@��@�z@�C@��@f�@�@�$@l"@�@�@�K@��@'�@�@
��@
�!@
��@
n�@
3�@	�o@	�@	��@	}�@	T�@	�@��@��@w�@>B@�@�@��@=@o@S@�@�@ߤ@ں@҉@ȴ@�R@��@ff@B[@�D@�N@��@�@��@��@��@�@}�@o @T�@-w@�@��@��@��@�e@��@��@q@]d@M@Ft@<�@�@x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
PB
6B
�B
�B
�B
6B
jB
PB
\B
}B
B
FB

B
EB
�B
�B
1B
B
�B
�B
7B
�B
�B
	B
WB
�B
�B
�B
�B
�B
qB
�B
�B	�%B	�OB	յB	żB	�AB	��B	�XB	�cB	�IB
�B
<B
�B
/�B
%zB
TB	��B
�B
t�B
zxB
�$B
�%B
�B
��B
��B
f�B
S@B
LdB
@�B
2-B
6�B
`\B
�B
׍B
�	B
��B
��B
�B
��B
�#B
��B
��B
}qB
v�B
l�B
@�B	��B	�B	��B	shB	l=B	b4B	P}B	=�B	0oB	!HB	 B��B��B�B�B��B�qB�HB��B��B��B�-B�B�
B�1B�-B��B�B	uB	aB��B	UB��B�B�XB	�B	5%B	8B	BAB	M�B	RoB	T�B	\)B	_�B	g�B	p!B	q�B	p�B	fLB	b�B	Y�B	]/B	i�B	�tB	��B	�uB	�B	��B	�B	�KB	�kB	��B	�'B	��B	�3B	�FB	��B	��B	��B	�B	�[B	�[B	��B	�B	��B	��B	��B	��B	��B	��B	�?B	��B	��B	�'B	�DB	�B	��B	��B	�lB	�JB	�dB	��B	�xB	��B	�B	��B	�,B	��B	��B	�B	�XB	�$B	�8B	��B	��B	�FB	�jB	��B	~�B	n/B	l�B	mCB	m]B	g�B	`vB	]�B	\�B	YB	W�B	V�B	S&B	Q�B	RoB	UMB	^OB	a�B	c�B	f�B	n/B	u%B	y>B	~�B	�{B	��B	��B	��B	��B	��B	�mB	�+B	�tB	�tB	��B	�zB	�B	��B	��B	�B	�BB	�BB	�B	��B	�B	��B	�@B	��B	�7B	��B	��B	�.B	�B	��B	��B	��B	յB	�+B	��B	�vB	ߊB	�B	��B	�HB	��B	�B	�BB	�vB	��B	�=B	�qB	�qB	��B	޸B	�B	�B	�&B	�tB	�B	��B	�B	�B	�B	�sB	��B	�XB	�
B	�B	�*B	�kB	�QB	�QB	��B	�=B	�]B	�B	��B	�iB	�iB	�5B	� B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�aB	��B	�B	�MB	�hB	�B	��B	�B	�B	��B	��B	�FB	�B	�LB	�2B	�B	�LB	��B	�lB	��B	�$B	�lB	��B	�	B	�XB	��B	�^B	�^B	�xB	�xB	�^B	�*B	��B	��B	�8B	��B	��B	�RB	�$B	�*B	��B	�B	��B	�6B	�"B	�B	�]B	�wB	��B	��B	�BB	��B	��B	�.B	��B	��B
 �B
�B
AB
�B
B
B
GB
-B
�B
GB
3B
�B
B
�B
B
�B
9B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
1B
�B
	�B

=B

�B

�B

XB

�B
�B
B
B
B
�B
�B
�B
0B
�B
�B

�B
xB
�B
JB
�B
dB
JB
xB
B
)B
^B
�B
0B
�B
�B
�B
B
jB
6B
�B
�B
�B
"B
<B
pB
vB
vB
�B
�B
.B
�B
B
�B
B
�B
�B
�B
?B
�B
EB
_B
EB
+B
EB
+B
yB
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
]B
CB
]B
�B
dB
IB
dB
IB
dB
5B
�B
�B
�B
pB
�B
 �B
!�B
!�B
!�B
!�B
!�B
!|B
"B
!�B
!�B
"4B
"�B
#�B
#�B
#TB
#�B
$ZB
$�B
%zB
%�B
&LB
&fB
&�B
&�B
&�B
&�B
'mB
'�B
(XB
)*B
)DB
)�B
*KB
+B
+B
*�B
*�B
+B
+kB
,B
,"B
,qB
,�B
-)B
,�B
-CB
.}B
/�B
/�B
/�B
/�B
0B
0!B
0�B
0�B
1B
1B
1AB
1[B
2GB
2-B
2aB
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
49B
4�B
5tB
5tB
5�B
5�B
5�B
5�B
6FB
6�B
6�B
7B
6�B
6�B
7B
7fB
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9	B
9>B
9�B
9�B
9�B
:*B
:*B
:^B
:^B
:DB
:�B
;B
;0B
;JB
;dB
<B
;�B
;�B
;�B
<6B
<jB
<�B
=B
=qB
=VB
=�B
=�B
>BB
>]B
>�B
>�B
>�B
>�B
>�B
?.B
?cB
?�B
?}B
?�B
?�B
@ B
@B
@OB
@OB
@iB
@iB
@�B
AB
A;B
AUB
AUB
A;B
AoB
BB
B'B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C-B
C�B
C�B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E9B
EB
EmB
E�B
FB
F?B
FtB
F�B
GB
G+B
G+B
GzB
G_B
G�B
HB
HfB
HfB
HKB
H�B
H�B
IB
I�B
I�B
I�B
JrB
J�B
K^B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
MPB
MPB
M�B
M�B
NB
NB
M�B
M�B
N�B
N�B
N�B
NpB
N�B
N�B
OB
OvB
O�B
O�B
PB
O�B
O�B
QNB
Q�B
Q�B
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S@B
S�B
TB
TB
T,B
TaB
TaB
T{B
T�B
T�B
T�B
T�B
UgB
UgB
U�B
U�B
V9B
VmB
VSB
VmB
VmB
W
B
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
Y�B
ZQB
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\�B
\�B
]/B
]~B
]�B
]�B
^B
^5B
^jB
^�B
_B
_!B
_VB
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bB
bhB
b�B
cB
c B
cTB
c�B
c�B
c�B
dB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
fB
fLB
gB
gmB
gRB
g�B
g�B
g�B
g�B
h
B
hsB
i*B
i_B
i�B
i�B
i�B
i�B
jeB
jB
jB
j�B
j�B
k�B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
oB
o5B
oOB
oOB
oiB
oiB
o�B
o�B
pB
pB
p;B
pUB
p�B
pUB
poB
q[B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r|B
r�B
s3B
s3B
s3B
sMB
sMB
s3B
sMB
shB
shB
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
vB
v+B
v+B
v`B
vFB
v`B
v`B
v`B
v�B
v�B
w�B
w�B
w�B
xB
xB
x8B
xB
xlB
x�B
y	B
x�B
y$B
y$B
yrB
y�B
y�B
y�B
y�B
z^B
z�B
z�B
z�B
{B
{JB
{B
{�B
{�B
|B
{�B
|B
|PB
|PB
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�4B
�OB
��B
��B
��B
� B
�UB
��B
��B
��B
�B
�[B
��B
��B
�GB
�GB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�MB
��B
��B
��B
�B
�B
�B
�B
�B
�9B
�mB
�SB
�mB
��B
��B
��B
�B
�%B
�?B
�?B
�tB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
PB
6B
�B
�B
�B
6B
jB
PB
\B
}B
B
FB

B
EB
�B
�B
1B
B
�B
�B
7B
�B
�B
	B
WB
�B
�B
�B
�B
�B
qB
�B
�B	�%B	�OB	յB	żB	�AB	��B	�XB	�cB	�IB
�B
<B
�B
/�B
%zB
TB	��B
�B
t�B
zxB
�$B
�%B
�B
��B
��B
f�B
S@B
LdB
@�B
2-B
6�B
`\B
�B
׍B
�	B
��B
��B
�B
��B
�#B
��B
��B
}qB
v�B
l�B
@�B	��B	�B	��B	shB	l=B	b4B	P}B	=�B	0oB	!HB	 B��B��B�B�B��B�qB�HB��B��B��B�-B�B�
B�1B�-B��B�B	uB	aB��B	UB��B�B�XB	�B	5%B	8B	BAB	M�B	RoB	T�B	\)B	_�B	g�B	p!B	q�B	p�B	fLB	b�B	Y�B	]/B	i�B	�tB	��B	�uB	�B	��B	�B	�KB	�kB	��B	�'B	��B	�3B	�FB	��B	��B	��B	�B	�[B	�[B	��B	�B	��B	��B	��B	��B	��B	��B	�?B	��B	��B	�'B	�DB	�B	��B	��B	�lB	�JB	�dB	��B	�xB	��B	�B	��B	�,B	��B	��B	�B	�XB	�$B	�8B	��B	��B	�FB	�jB	��B	~�B	n/B	l�B	mCB	m]B	g�B	`vB	]�B	\�B	YB	W�B	V�B	S&B	Q�B	RoB	UMB	^OB	a�B	c�B	f�B	n/B	u%B	y>B	~�B	�{B	��B	��B	��B	��B	��B	�mB	�+B	�tB	�tB	��B	�zB	�B	��B	��B	�B	�BB	�BB	�B	��B	�B	��B	�@B	��B	�7B	��B	��B	�.B	�B	��B	��B	��B	յB	�+B	��B	�vB	ߊB	�B	��B	�HB	��B	�B	�BB	�vB	��B	�=B	�qB	�qB	��B	޸B	�B	�B	�&B	�tB	�B	��B	�B	�B	�B	�sB	��B	�XB	�
B	�B	�*B	�kB	�QB	�QB	��B	�=B	�]B	�B	��B	�iB	�iB	�5B	� B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�aB	��B	�B	�MB	�hB	�B	��B	�B	�B	��B	��B	�FB	�B	�LB	�2B	�B	�LB	��B	�lB	��B	�$B	�lB	��B	�	B	�XB	��B	�^B	�^B	�xB	�xB	�^B	�*B	��B	��B	�8B	��B	��B	�RB	�$B	�*B	��B	�B	��B	�6B	�"B	�B	�]B	�wB	��B	��B	�BB	��B	��B	�.B	��B	��B
 �B
�B
AB
�B
B
B
GB
-B
�B
GB
3B
�B
B
�B
B
�B
9B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
1B
�B
	�B

=B

�B

�B

XB

�B
�B
B
B
B
�B
�B
�B
0B
�B
�B

�B
xB
�B
JB
�B
dB
JB
xB
B
)B
^B
�B
0B
�B
�B
�B
B
jB
6B
�B
�B
�B
"B
<B
pB
vB
vB
�B
�B
.B
�B
B
�B
B
�B
�B
�B
?B
�B
EB
_B
EB
+B
EB
+B
yB
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
]B
CB
]B
�B
dB
IB
dB
IB
dB
5B
�B
�B
�B
pB
�B
 �B
!�B
!�B
!�B
!�B
!�B
!|B
"B
!�B
!�B
"4B
"�B
#�B
#�B
#TB
#�B
$ZB
$�B
%zB
%�B
&LB
&fB
&�B
&�B
&�B
&�B
'mB
'�B
(XB
)*B
)DB
)�B
*KB
+B
+B
*�B
*�B
+B
+kB
,B
,"B
,qB
,�B
-)B
,�B
-CB
.}B
/�B
/�B
/�B
/�B
0B
0!B
0�B
0�B
1B
1B
1AB
1[B
2GB
2-B
2aB
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
49B
4�B
5tB
5tB
5�B
5�B
5�B
5�B
6FB
6�B
6�B
7B
6�B
6�B
7B
7fB
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9	B
9>B
9�B
9�B
9�B
:*B
:*B
:^B
:^B
:DB
:�B
;B
;0B
;JB
;dB
<B
;�B
;�B
;�B
<6B
<jB
<�B
=B
=qB
=VB
=�B
=�B
>BB
>]B
>�B
>�B
>�B
>�B
>�B
?.B
?cB
?�B
?}B
?�B
?�B
@ B
@B
@OB
@OB
@iB
@iB
@�B
AB
A;B
AUB
AUB
A;B
AoB
BB
B'B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C-B
C�B
C�B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E9B
EB
EmB
E�B
FB
F?B
FtB
F�B
GB
G+B
G+B
GzB
G_B
G�B
HB
HfB
HfB
HKB
H�B
H�B
IB
I�B
I�B
I�B
JrB
J�B
K^B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
MPB
MPB
M�B
M�B
NB
NB
M�B
M�B
N�B
N�B
N�B
NpB
N�B
N�B
OB
OvB
O�B
O�B
PB
O�B
O�B
QNB
Q�B
Q�B
Q�B
RB
R B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S@B
S�B
TB
TB
T,B
TaB
TaB
T{B
T�B
T�B
T�B
T�B
UgB
UgB
U�B
U�B
V9B
VmB
VSB
VmB
VmB
W
B
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
X�B
Y�B
ZQB
Z�B
Z�B
[#B
[�B
\B
\B
\)B
\�B
\�B
]/B
]~B
]�B
]�B
^B
^5B
^jB
^�B
_B
_!B
_VB
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bB
bhB
b�B
cB
c B
cTB
c�B
c�B
c�B
dB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
fB
fLB
gB
gmB
gRB
g�B
g�B
g�B
g�B
h
B
hsB
i*B
i_B
i�B
i�B
i�B
i�B
jeB
jB
jB
j�B
j�B
k�B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
oB
o5B
oOB
oOB
oiB
oiB
o�B
o�B
pB
pB
p;B
pUB
p�B
pUB
poB
q[B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r|B
r�B
s3B
s3B
s3B
sMB
sMB
s3B
sMB
shB
shB
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
vB
v+B
v+B
v`B
vFB
v`B
v`B
v`B
v�B
v�B
w�B
w�B
w�B
xB
xB
x8B
xB
xlB
x�B
y	B
x�B
y$B
y$B
yrB
y�B
y�B
y�B
y�B
z^B
z�B
z�B
z�B
{B
{JB
{B
{�B
{�B
|B
{�B
|B
|PB
|PB
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�4B
�OB
��B
��B
��B
� B
�UB
��B
��B
��B
�B
�[B
��B
��B
�GB
�GB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�MB
��B
��B
��B
�B
�B
�B
�B
�B
�9B
�mB
�SB
�mB
��B
��B
��B
�B
�%B
�?B
�?B
�tB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105240  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192121  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192121  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192121                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042128  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042128  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                