CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:36:53Z creation;2022-06-04T17:36:54Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173653  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               OA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�j���j1   @�j�z�G�@0����o�d ���S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B33B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C33C�fC  C
  C  C�C  C  C�fC�fC�fC�fC  C  C   C"  C$  C&  C(�C*�C,�C.  C/�fC1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�&fC��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�\)B���B���B�B���B���B���B���B���B���B���C��C.C�GC��C	��C��C{C��C��C�GC�GC�GC�GC��C��C��C!��C#��C%��C({C*{C,{C-��C/�GC1�GC3�GC5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CL{CN{CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C|{C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�#�C��C��C��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�DuDu~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\D�D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aɤ�Aɨ$AɧRAɨ$AɦLAɥFAɧ�Aɦ�Aɩ*Aɬ=Aɦ�Aɜ�AɁoA�8�A��3AȅSA�g�A�`vA�FA��A��A�.A�iDA�"�A�oA���A���AĿ�AĮAĐbA�zA�TaA�8�A�#�A�A�~A��A��A��A��2A���A�̘A��A��A��A���A�A���A��A��A���Aõ�AåA�|PA�aA���A�r�A���A�DA�՛A���A�!bA�<�A�(�A�>A�TaA��WA��	A���A�iA�K)A���A�cTA��'A���A�a|A��BA��A�r|A�MjA�#:A���A�9�A�L�A��A���A�HKA���A��A�v�A��2A�j�A��A���A���A���A��A�YA���A���A�A��qA�g8A�[�A�v�A|�sAy�PAv�!AsqAo�Ag�Af!Ad%�A`��AZ��AX��AV�AT�AP�AOl�AK��AJ�AIF�AF��AD!ACV�ABt�A@҉A?  A>xlA@��A?�}A?kQA?�A=c�A:>BA6�A.�A-GEA+�=A*k�A)�hA(RTA'��A'��A'�A&�A%$A$0�A"�DA"!�A!��A!JA �wA �\A!��A$��A'�A(33A) �A*q�A+�A,xA+��A*�[A)�A(�A&��A&A&B[A%�nA$�rA$��A$FA#��A#%�A"��A"n�A"/A!�tA!
�A ��A JAƨAg8A[WA_AF�A
=A��A^�AW?A/�AA��AtTA�Ae,A�A�AM�A`BA��A�A�6A�VAMA��A#:A��AƨAK^A�[AZ�A�"A�^A�mA��AMA��AU2A�ADgA�	A��A��A�	A��AɆA�A�AA�RA|�A,�A�&AY�AA�3A��A+A&�A��A��A
E�A	?A�zA��A��A��A-A�AϫAm�A�ASA��Ac�AɆA��A��A�UA��A�aA[�AU�A�A@OA�tA ��A 7LA T�A :�A �@�ƨ@�"h@��@��@��.@�w�@���@�U�@�e�@���@�%F@��@�1@�#:@��@�*0@��@���@�v�@�Q�@�'R@�g�@��@�6z@�D�@��"@��@�l�@�ѷ@���@�O@�e@�d�@�Mj@�YK@�j@��@�J�@���@�&@��M@��@��@�:�@�t�@�oi@���@�&@ޫ6@�~@ݴ�@�?}@�
=@ܰ!@�_�@��@۾w@�F@�%�@�� @م@��s@�h
@׈f@ְ!@��@��W@�ݘ@�ƨ@շ�@�x�@�@�D�@��@��@�4n@ҵ�@���@�;d@�`B@ѥ�@ѩ*@�Y@��8@е�@�r�@�a|@���@��@�V@�ݘ@�8@��X@�)�@���@˫�@�E9@Ȟ@���@ɲ-@�@O@ȸR@���@ȉ�@�^5@��@Ǫ�@�%F@��@ƕ@Ł�@��@���@�V�@�!�@�Q@�(�@�`B@��@��@�8�@�1@��@��>@��7@��@��o@�֡@�"�@��v@�?@���@�qv@�O@���@���@��@�H�@�GE@��&@��@@�`B@�8�@�	l@��F@��@�33@��X@��@�|@�;d@���@�	@��t@�x�@��@��,@��@��u@���@�`�@�?}@���@�ff@�I�@��@�e,@�7L@��@��[@�tT@�H�@�Ta@�@��0@�J#@�S@��j@��@�^5@�S�@��Z@���@��6@�9X@��D@��@��@��K@���@�33@�q@�$t@���@�1'@��@�ƨ@��:@�W?@�@��r@�-@���@���@�C�@��@��@��P@��M@�i�@���@���@��@�{�@�-�@��@��m@��
@��H@��V@�Mj@��<@���@��.@�M@���@�@���@��A@�1�@��3@�K�@��@��b@�ff@�-�@�b@��Z@��g@���@�\)@�ѷ@���@�u%@�PH@� �@�خ@�Mj@��2@���@�?�@�-@�#:@�u@��W@���@�rG@�W?@�Dg@�4�@��@��|@���@�|�@�;�@��@���@��>@��;@��6@���@�zx@�s�@�W?@��P@�Ɇ@���@��Y@�\�@�u@�o�@���@��?@���@��@��D@���@���@�33@�&�@��@�%@��@��@���@���@���@�.�@�G@��@��K@�m]@��@��s@��@�Ov@���@��"@�F�@��@��@��p@��r@�Q@�4n@��@���@�Vm@�,�@���@�p;@�-@��.@��@���@��@���@�`B@�33@���@�xl@�E�@��@��@���@���@�L�@��@���@��@���@�?@��@��@��k@�v`@�Dg@��@��"@��v@���@�j@�)�@��X@�Q�@��@�9X@���@���@��{@�qv@�F�@�&@��@��H@���@�@~}V@}N<@|�@{�
@{��@{8@{1�@{!-@{ i@z��@zOv@y��@yT�@x��@w�W@v��@v� @u�C@u5�@t�K@t��@t�j@t�O@t�Y@s�g@r��@r\�@r1�@qo @pU2@o�@n�s@nJ@mY�@l�5@l��@lr�@lC-@k�V@k$t@jں@j�!@j}V@je@i��@iN<@h�@h9X@h�@h%�@h/�@h�@h  @g��@g�@g��@g_p@g@O@g�@f��@f�'@f�@d�f@dQ�@d4n@d�@c��@cx@c�@b��@b�@a�t@aO�@`Ĝ@`!@_�F@_qv@_E9@_�@^��@^��@^^5@]��@]G�@\�@\c�@\ �@[��@[@O@Z�R@Z:*@Z{@YVm@Xx@W��@WiD@WX�@W9�@W&@V�H@Vl�@V$�@V
�@U��@TV�@S;d@R��@RQ@R!�@Q��@Q��@Qa�@P�5@P��@P�@P|�@Pm�@PI�@P�@Oخ@O�f@OK�@Nں@N3�@M��@L`�@K�@Kl�@KE9@J�,@Jff@I��@I��@Ia�@I@HĜ@H�@HS�@HD�@H4n@G��@G.I@F�X@F��@F��@F}V@F}V@Fs�@Fh
@E��@E��@D�5@D�@D9X@D@Cخ@CC�@B��@BGE@A��@AVm@A�@@�@@�@?�@?iD@?,�@>��@>��@>V@>=q@>($@>	@>J@=�@=�h@=[W@=�@<oi@<A�@;�W@;iD@;)_@:;�@9�H@9[W@9=�@9�@8��@8��@8g8@8Z@8D�@7��@7�&@7t�@7Y@6�@6҉@6�B@6��@6��@6�L@6��@61�@5G�@4�f@4�@4]d@4b@3��@3Z�@2�@2�@2�h@2��@2�@1�X@1(�@1�@0�?@0�@0[�@/�@/�g@/�*@/F�@.��@.��@.�@.p;@.3�@.�@-��@-�@-�3@-rG@-IR@-�@,��@,�?@,Xy@,b@+�w@+t�@+/�@+�@*͟@*�h@*��@*:*@*u@)��@)a�@)-w@(�P@(�@(�O@(h�@(h�@(]d@(M@'˒@'{J@';d@'!-@&�m@&?@&u@%��@%5�@%!�@% \@%�@%�@$�E@$�_@$�o@$C-@#��@#S@"�!@"i�@"H�@"�@!�>@!�N@!��@ �|@ Ft@ "h@�;@��@l�@O@1�@�c@ߤ@��@ȴ@��@�@��@i�@R�@1�@�@�h@<6@�@�/@�u@h�@!@��@��@a@A�@$t@�@�"@��@M�@�@�@x�@S&@&�@��@Ɇ@��@h�@Q�@I�@�+@�6@��@�V@��@��@��@��@��@�	@��@qv@.I@��@�]@�'@p;@.�@�3@��@�=@�"@zx@rG@IR@��@��@,=@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aɤ�Aɨ$AɧRAɨ$AɦLAɥFAɧ�Aɦ�Aɩ*Aɬ=Aɦ�Aɜ�AɁoA�8�A��3AȅSA�g�A�`vA�FA��A��A�.A�iDA�"�A�oA���A���AĿ�AĮAĐbA�zA�TaA�8�A�#�A�A�~A��A��A��A��2A���A�̘A��A��A��A���A�A���A��A��A���Aõ�AåA�|PA�aA���A�r�A���A�DA�՛A���A�!bA�<�A�(�A�>A�TaA��WA��	A���A�iA�K)A���A�cTA��'A���A�a|A��BA��A�r|A�MjA�#:A���A�9�A�L�A��A���A�HKA���A��A�v�A��2A�j�A��A���A���A���A��A�YA���A���A�A��qA�g8A�[�A�v�A|�sAy�PAv�!AsqAo�Ag�Af!Ad%�A`��AZ��AX��AV�AT�AP�AOl�AK��AJ�AIF�AF��AD!ACV�ABt�A@҉A?  A>xlA@��A?�}A?kQA?�A=c�A:>BA6�A.�A-GEA+�=A*k�A)�hA(RTA'��A'��A'�A&�A%$A$0�A"�DA"!�A!��A!JA �wA �\A!��A$��A'�A(33A) �A*q�A+�A,xA+��A*�[A)�A(�A&��A&A&B[A%�nA$�rA$��A$FA#��A#%�A"��A"n�A"/A!�tA!
�A ��A JAƨAg8A[WA_AF�A
=A��A^�AW?A/�AA��AtTA�Ae,A�A�AM�A`BA��A�A�6A�VAMA��A#:A��AƨAK^A�[AZ�A�"A�^A�mA��AMA��AU2A�ADgA�	A��A��A�	A��AɆA�A�AA�RA|�A,�A�&AY�AA�3A��A+A&�A��A��A
E�A	?A�zA��A��A��A-A�AϫAm�A�ASA��Ac�AɆA��A��A�UA��A�aA[�AU�A�A@OA�tA ��A 7LA T�A :�A �@�ƨ@�"h@��@��@��.@�w�@���@�U�@�e�@���@�%F@��@�1@�#:@��@�*0@��@���@�v�@�Q�@�'R@�g�@��@�6z@�D�@��"@��@�l�@�ѷ@���@�O@�e@�d�@�Mj@�YK@�j@��@�J�@���@�&@��M@��@��@�:�@�t�@�oi@���@�&@ޫ6@�~@ݴ�@�?}@�
=@ܰ!@�_�@��@۾w@�F@�%�@�� @م@��s@�h
@׈f@ְ!@��@��W@�ݘ@�ƨ@շ�@�x�@�@�D�@��@��@�4n@ҵ�@���@�;d@�`B@ѥ�@ѩ*@�Y@��8@е�@�r�@�a|@���@��@�V@�ݘ@�8@��X@�)�@���@˫�@�E9@Ȟ@���@ɲ-@�@O@ȸR@���@ȉ�@�^5@��@Ǫ�@�%F@��@ƕ@Ł�@��@���@�V�@�!�@�Q@�(�@�`B@��@��@�8�@�1@��@��>@��7@��@��o@�֡@�"�@��v@�?@���@�qv@�O@���@���@��@�H�@�GE@��&@��@@�`B@�8�@�	l@��F@��@�33@��X@��@�|@�;d@���@�	@��t@�x�@��@��,@��@��u@���@�`�@�?}@���@�ff@�I�@��@�e,@�7L@��@��[@�tT@�H�@�Ta@�@��0@�J#@�S@��j@��@�^5@�S�@��Z@���@��6@�9X@��D@��@��@��K@���@�33@�q@�$t@���@�1'@��@�ƨ@��:@�W?@�@��r@�-@���@���@�C�@��@��@��P@��M@�i�@���@���@��@�{�@�-�@��@��m@��
@��H@��V@�Mj@��<@���@��.@�M@���@�@���@��A@�1�@��3@�K�@��@��b@�ff@�-�@�b@��Z@��g@���@�\)@�ѷ@���@�u%@�PH@� �@�خ@�Mj@��2@���@�?�@�-@�#:@�u@��W@���@�rG@�W?@�Dg@�4�@��@��|@���@�|�@�;�@��@���@��>@��;@��6@���@�zx@�s�@�W?@��P@�Ɇ@���@��Y@�\�@�u@�o�@���@��?@���@��@��D@���@���@�33@�&�@��@�%@��@��@���@���@���@�.�@�G@��@��K@�m]@��@��s@��@�Ov@���@��"@�F�@��@��@��p@��r@�Q@�4n@��@���@�Vm@�,�@���@�p;@�-@��.@��@���@��@���@�`B@�33@���@�xl@�E�@��@��@���@���@�L�@��@���@��@���@�?@��@��@��k@�v`@�Dg@��@��"@��v@���@�j@�)�@��X@�Q�@��@�9X@���@���@��{@�qv@�F�@�&@��@��H@���@�@~}V@}N<@|�@{�
@{��@{8@{1�@{!-@{ i@z��@zOv@y��@yT�@x��@w�W@v��@v� @u�C@u5�@t�K@t��@t�j@t�O@t�Y@s�g@r��@r\�@r1�@qo @pU2@o�@n�s@nJ@mY�@l�5@l��@lr�@lC-@k�V@k$t@jں@j�!@j}V@je@i��@iN<@h�@h9X@h�@h%�@h/�@h�@h  @g��@g�@g��@g_p@g@O@g�@f��@f�'@f�@d�f@dQ�@d4n@d�@c��@cx@c�@b��@b�@a�t@aO�@`Ĝ@`!@_�F@_qv@_E9@_�@^��@^��@^^5@]��@]G�@\�@\c�@\ �@[��@[@O@Z�R@Z:*@Z{@YVm@Xx@W��@WiD@WX�@W9�@W&@V�H@Vl�@V$�@V
�@U��@TV�@S;d@R��@RQ@R!�@Q��@Q��@Qa�@P�5@P��@P�@P|�@Pm�@PI�@P�@Oخ@O�f@OK�@Nں@N3�@M��@L`�@K�@Kl�@KE9@J�,@Jff@I��@I��@Ia�@I@HĜ@H�@HS�@HD�@H4n@G��@G.I@F�X@F��@F��@F}V@F}V@Fs�@Fh
@E��@E��@D�5@D�@D9X@D@Cخ@CC�@B��@BGE@A��@AVm@A�@@�@@�@?�@?iD@?,�@>��@>��@>V@>=q@>($@>	@>J@=�@=�h@=[W@=�@<oi@<A�@;�W@;iD@;)_@:;�@9�H@9[W@9=�@9�@8��@8��@8g8@8Z@8D�@7��@7�&@7t�@7Y@6�@6҉@6�B@6��@6��@6�L@6��@61�@5G�@4�f@4�@4]d@4b@3��@3Z�@2�@2�@2�h@2��@2�@1�X@1(�@1�@0�?@0�@0[�@/�@/�g@/�*@/F�@.��@.��@.�@.p;@.3�@.�@-��@-�@-�3@-rG@-IR@-�@,��@,�?@,Xy@,b@+�w@+t�@+/�@+�@*͟@*�h@*��@*:*@*u@)��@)a�@)-w@(�P@(�@(�O@(h�@(h�@(]d@(M@'˒@'{J@';d@'!-@&�m@&?@&u@%��@%5�@%!�@% \@%�@%�@$�E@$�_@$�o@$C-@#��@#S@"�!@"i�@"H�@"�@!�>@!�N@!��@ �|@ Ft@ "h@�;@��@l�@O@1�@�c@ߤ@��@ȴ@��@�@��@i�@R�@1�@�@�h@<6@�@�/@�u@h�@!@��@��@a@A�@$t@�@�"@��@M�@�@�@x�@S&@&�@��@Ɇ@��@h�@Q�@I�@�+@�6@��@�V@��@��@��@��@��@�	@��@qv@.I@��@�]@�'@p;@.�@�3@��@�=@�"@zx@rG@IR@��@��@,=@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�[B�vB�'B�B��B��B�AB�B�?B��B�"B	�B	&�B	-�B	,�B	0oB	4�B	YKB	�"B
�B
ZkB
`�B
cnB
e`B
h$B
g�B
h�B
h�B
i_B
j�B
i�B
h�B
h�B
k6B
kkB
jKB
j�B
k6B
kB
i�B
qvB
y�B
~�B
~wB
B
��B
�pB
�4B
�B
��B
�7B
��B
��B
�iB
��B
��B*B-wB9�B@�BD�BSuB]�Bb�Bb�Be`Bh�Bl�BoBpoB��B��B��B��By$BT,BL�BV�BY�B^�B[�Bm�B��B`�BM�B<�B0�B~B
�B
��B
�(B
��B
��B
}B
p�B
\�B
C�B
7�B
!�B
�B
	7B
;B	�B	��B	�ZB	�B	��B	��B	e,B	\]B	R�B	LdB	9�B	4TB	/B	%�B	�B	�B	�B	
�B	,B	B	�B	�B	�B	�B	
	B	"NB	}B	��B	��B	��B	�jB	x�B	O�B	 B�B��BڠBܒB�QB�!B�}B�^B��B	 �B	�B		�B	^B	
�B	dB	�B	�B	)B	n�B	��B	�qB	�"B
]B
,�B
HKB
H�B
H�B
H�B
C�B
AB
A;B
I�B
I�B
HKB
I�B
J�B
IRB
M6B
Q B
Q B
Q�B
R B
P}B
OBB
P}B
P}B
P}B
Q�B
RoB
TB
U�B
T�B
T{B
TB
TFB
UB
V�B
W$B
R�B
Q4B
D�B
9XB
3MB
+�B
+kB
.B
1�B
1vB
(�B
)�B
4nB
5%B
:�B
;JB
:DB
9XB
:�B
:*B
;dB
:�B
9>B
6`B
6zB
8�B
4�B
4nB
3�B
5tB
9>B
9>B
6�B
:�B
C-B
B�B
A�B
?�B
>]B
=�B
>�B
=�B
;�B
9	B
4TB
/�B
,�B
%�B
�B
{B
�B
�B
�B
�B
�B
0B
	�B
�B
�B
 B
 �B
B
�B
B
�B
mB
uB
<B
jB
~B
�B
�B
{B
 OB
'B
1B
�B
+B
�B	�qB	�B	��B	�/B	�QB	�eB	�B	�B	�B	��B
PB
B
�B
�B
 B
 B
B
�B
hB
�B
4B
�B
B
B
�B

XB
	B
�B
�B
B
SB
�B
�B
�B
 B
 OB	�cB	�.B
�B	��B	��B	�VB	��B	�xB	��B	�DB	�^B	�DB	��B	��B	��B	��B	�B	�LB	��B	�`B	�FB	��B	�B	�rB	��B	��B	��B	�B	�PB	�"B	�"B	��B	�"B	��B	�dB	�B	�xB	�rB	�}B	�B	��B	��B
{B
�B
�B
�B
YB
�B

�B

=B

XB
	�B
KB
�B
�B
�B
SB
GB	�PB	��B	��B
�B
�B
[B
�B
+B
B
	lB
	�B
	�B
	�B
	�B
�B
YB
�B	��B	��B	�.B	�cB	�<B	��B	�BB	��B	�jB	��B	��B	��B	�"B	�6B	��B
�B
B
3B
3B
3B
�B
B
 B	�(B
 OB
�B
�B
�B
�B
�B
�B
B
aB
�B
 �B	��B	��B	��B	��B	��B	�B	��B	�<B	��B	��B	��B	�B	��B	�B
 �B
 B
'B
�B
AB
GB
B
�B
B
B
	�B
�B
�B
B
dB
�B
�B
dB
^B
^B
�B
JB
^B
�B
�B
~B
vB
bB
\B
(B
�B
&B
@B
�B
:B
B
[B
B
�B
�B
�B
�B
B
�B
�B
B
FB
�B
�B
�B
�B
B
,B
�B
�B
�B
MB
2B
{B
B
{B
2B
�B
�B
�B
�B
2B
�B
MB
2B
�B
�B
aB
FB
aB
FB
,B
�B
aB
�B
�B
$B
$B
�B
�B
YB
�B
mB
�B
mB
9B
$B
�B
�B
�B
�B
+B
_B
�B
yB
�B
�B
eB
1B
B
eB
�B
KB
�B
�B
�B
�B
B
eB
�B
KB
B
eB
�B
#B
=B
�B
�B
	B
�B
�B
CB
�B
CB
�B
dB
B
~B
�B
�B
�B
�B
B
jB
�B
VB
 BB
!B
 �B
!HB
"�B
# B
"�B
#�B
#�B
#�B
$B
$�B
#�B
$�B
$�B
$�B
$�B
$tB
%B
%`B
%�B
&B
%�B
%�B
&2B
%�B
&fB
&LB
&�B
&�B
&�B
'�B
'B
'mB
'�B
'RB
'�B
(>B
(XB
(�B
($B
)_B
)_B
)�B
)DB
*B
)�B
*B
*B
*�B
,�B
,�B
-�B
,WB
)�B
*KB
*�B
+B
,qB
-�B
/OB
/B
.�B
-�B
.�B
-B
,qB
+�B
+6B
*�B
*B
*�B
+B
+B
+B
*�B
*�B
+B
*�B
*�B
+�B
,"B
+�B
+�B
+kB
,=B
,B
,qB
-�B
-CB
-�B
-]B
,�B
-�B
-�B
-�B
-�B
-wB
-�B
/ B
.�B
.�B
0�B
2�B
3�B
49B
4�B
4�B
5?B
5B
5ZB
6+B
7�B
9�B
<B
<PB
<�B
<jB
<�B
=�B
=�B
=�B
>wB
>]B
=�B
=�B
>(B
>�B
=�B
=�B
=�B
>BB
>BB
=�B
=�B
=�B
=�B
=VB
=�B
=�B
>�B
>(B
>wB
=�B
>BB
=�B
>B
>BB
>�B
>�B
>�B
?B
?cB
>�B
?.B
?cB
?}B
AB
A B
AUB
@�B
A�B
AB
A B
A�B
B�B
B�B
B[B
B�B
C�B
C�B
C�B
D3B
C�B
C�B
CGB
C�B
DB
C�B
DgB
C�B
C�B
C�B
C�B
C�B
C{B
C�B
DMB
C�B
EB
E�B
F%B
E�B
FtB
F�B
F�B
F�B
GB
G_B
F�B
GEB
G_B
F�B
GEB
G�B
HB
IlB
J	B
I�B
J#B
JXB
JrB
J	B
K�B
L~B
MB
MjB
M�B
M�B
M�B
N<B
N�B
N"B
N�B
O�B
O�B
PbB
PHB
Q B
QhB
P�B
Q�B
RoB
R�B
S[B
S�B
T,B
S�B
U�B
UMB
U�B
VB
V�B
V�B
V�B
WYB
WYB
X�B
XEB
X�B
X_B
X�B
X�B
YeB
Y�B
Y�B
Y�B
[�B
]�B
]�B
]IB
^5B
^B
]IB
^B
]�B
]�B
^OB
^�B
_B
_B
_VB
_�B
_�B
_VB
_�B
`'B
_�B
`B
_�B
_VB
_pB
_pB
_;B
_!B
_�B
_�B
`BB
`BB
`�B
`\B
`�B
`�B
`�B
abB
b�B
b�B
c B
cB
b�B
cTB
c:B
cTB
cnB
cnB
c�B
c�B
c�B
d&B
d�B
d�B
eB
d�B
d�B
e�B
e,B
e�B
e�B
fLB
f�B
f�B
f�B
fLB
f2B
e�B
ffB
f2B
f�B
f�B
g8B
h�B
h�B
h�B
iDB
i�B
i�B
iyB
i�B
jB
j�B
jB
j�B
j�B
k�B
k�B
kkB
l=B
lqB
k�B
lqB
l"B
m)B
m�B
mwB
m�B
nB
n�B
ncB
nIB
n}B
o5B
o5B
n�B
n�B
oB
o B
o�B
o�B
o�B
o�B
p�B
p�B
poB
p�B
q'B
p�B
qvB
q�B
q[B
q�B
rB
r�B
r�B
q�B
rB
r�B
r�B
s3B
s3B
s�B
s�B
s�B
tTB
tB
t9B
tB
t�B
tTB
t�B
uB
t�B
t�B
uB
t�B
u%B
t�B
t�B
u%B
t�B
u?B
uZB
u�B
uZB
u�B
u�B
u�B
v�B
vFB
v�B
vzB
v�B
v`B
v�B
wB
wfB
xB
xB
x8B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B�[B�vB�'B�B��B��B�AB�B�?B��B�"B	�B	&�B	-�B	,�B	0oB	4�B	YKB	�"B
�B
ZkB
`�B
cnB
e`B
h$B
g�B
h�B
h�B
i_B
j�B
i�B
h�B
h�B
k6B
kkB
jKB
j�B
k6B
kB
i�B
qvB
y�B
~�B
~wB
B
��B
�pB
�4B
�B
��B
�7B
��B
��B
�iB
��B
��B*B-wB9�B@�BD�BSuB]�Bb�Bb�Be`Bh�Bl�BoBpoB��B��B��B��By$BT,BL�BV�BY�B^�B[�Bm�B��B`�BM�B<�B0�B~B
�B
��B
�(B
��B
��B
}B
p�B
\�B
C�B
7�B
!�B
�B
	7B
;B	�B	��B	�ZB	�B	��B	��B	e,B	\]B	R�B	LdB	9�B	4TB	/B	%�B	�B	�B	�B	
�B	,B	B	�B	�B	�B	�B	
	B	"NB	}B	��B	��B	��B	�jB	x�B	O�B	 B�B��BڠBܒB�QB�!B�}B�^B��B	 �B	�B		�B	^B	
�B	dB	�B	�B	)B	n�B	��B	�qB	�"B
]B
,�B
HKB
H�B
H�B
H�B
C�B
AB
A;B
I�B
I�B
HKB
I�B
J�B
IRB
M6B
Q B
Q B
Q�B
R B
P}B
OBB
P}B
P}B
P}B
Q�B
RoB
TB
U�B
T�B
T{B
TB
TFB
UB
V�B
W$B
R�B
Q4B
D�B
9XB
3MB
+�B
+kB
.B
1�B
1vB
(�B
)�B
4nB
5%B
:�B
;JB
:DB
9XB
:�B
:*B
;dB
:�B
9>B
6`B
6zB
8�B
4�B
4nB
3�B
5tB
9>B
9>B
6�B
:�B
C-B
B�B
A�B
?�B
>]B
=�B
>�B
=�B
;�B
9	B
4TB
/�B
,�B
%�B
�B
{B
�B
�B
�B
�B
�B
0B
	�B
�B
�B
 B
 �B
B
�B
B
�B
mB
uB
<B
jB
~B
�B
�B
{B
 OB
'B
1B
�B
+B
�B	�qB	�B	��B	�/B	�QB	�eB	�B	�B	�B	��B
PB
B
�B
�B
 B
 B
B
�B
hB
�B
4B
�B
B
B
�B

XB
	B
�B
�B
B
SB
�B
�B
�B
 B
 OB	�cB	�.B
�B	��B	��B	�VB	��B	�xB	��B	�DB	�^B	�DB	��B	��B	��B	��B	�B	�LB	��B	�`B	�FB	��B	�B	�rB	��B	��B	��B	�B	�PB	�"B	�"B	��B	�"B	��B	�dB	�B	�xB	�rB	�}B	�B	��B	��B
{B
�B
�B
�B
YB
�B

�B

=B

XB
	�B
KB
�B
�B
�B
SB
GB	�PB	��B	��B
�B
�B
[B
�B
+B
B
	lB
	�B
	�B
	�B
	�B
�B
YB
�B	��B	��B	�.B	�cB	�<B	��B	�BB	��B	�jB	��B	��B	��B	�"B	�6B	��B
�B
B
3B
3B
3B
�B
B
 B	�(B
 OB
�B
�B
�B
�B
�B
�B
B
aB
�B
 �B	��B	��B	��B	��B	��B	�B	��B	�<B	��B	��B	��B	�B	��B	�B
 �B
 B
'B
�B
AB
GB
B
�B
B
B
	�B
�B
�B
B
dB
�B
�B
dB
^B
^B
�B
JB
^B
�B
�B
~B
vB
bB
\B
(B
�B
&B
@B
�B
:B
B
[B
B
�B
�B
�B
�B
B
�B
�B
B
FB
�B
�B
�B
�B
B
,B
�B
�B
�B
MB
2B
{B
B
{B
2B
�B
�B
�B
�B
2B
�B
MB
2B
�B
�B
aB
FB
aB
FB
,B
�B
aB
�B
�B
$B
$B
�B
�B
YB
�B
mB
�B
mB
9B
$B
�B
�B
�B
�B
+B
_B
�B
yB
�B
�B
eB
1B
B
eB
�B
KB
�B
�B
�B
�B
B
eB
�B
KB
B
eB
�B
#B
=B
�B
�B
	B
�B
�B
CB
�B
CB
�B
dB
B
~B
�B
�B
�B
�B
B
jB
�B
VB
 BB
!B
 �B
!HB
"�B
# B
"�B
#�B
#�B
#�B
$B
$�B
#�B
$�B
$�B
$�B
$�B
$tB
%B
%`B
%�B
&B
%�B
%�B
&2B
%�B
&fB
&LB
&�B
&�B
&�B
'�B
'B
'mB
'�B
'RB
'�B
(>B
(XB
(�B
($B
)_B
)_B
)�B
)DB
*B
)�B
*B
*B
*�B
,�B
,�B
-�B
,WB
)�B
*KB
*�B
+B
,qB
-�B
/OB
/B
.�B
-�B
.�B
-B
,qB
+�B
+6B
*�B
*B
*�B
+B
+B
+B
*�B
*�B
+B
*�B
*�B
+�B
,"B
+�B
+�B
+kB
,=B
,B
,qB
-�B
-CB
-�B
-]B
,�B
-�B
-�B
-�B
-�B
-wB
-�B
/ B
.�B
.�B
0�B
2�B
3�B
49B
4�B
4�B
5?B
5B
5ZB
6+B
7�B
9�B
<B
<PB
<�B
<jB
<�B
=�B
=�B
=�B
>wB
>]B
=�B
=�B
>(B
>�B
=�B
=�B
=�B
>BB
>BB
=�B
=�B
=�B
=�B
=VB
=�B
=�B
>�B
>(B
>wB
=�B
>BB
=�B
>B
>BB
>�B
>�B
>�B
?B
?cB
>�B
?.B
?cB
?}B
AB
A B
AUB
@�B
A�B
AB
A B
A�B
B�B
B�B
B[B
B�B
C�B
C�B
C�B
D3B
C�B
C�B
CGB
C�B
DB
C�B
DgB
C�B
C�B
C�B
C�B
C�B
C{B
C�B
DMB
C�B
EB
E�B
F%B
E�B
FtB
F�B
F�B
F�B
GB
G_B
F�B
GEB
G_B
F�B
GEB
G�B
HB
IlB
J	B
I�B
J#B
JXB
JrB
J	B
K�B
L~B
MB
MjB
M�B
M�B
M�B
N<B
N�B
N"B
N�B
O�B
O�B
PbB
PHB
Q B
QhB
P�B
Q�B
RoB
R�B
S[B
S�B
T,B
S�B
U�B
UMB
U�B
VB
V�B
V�B
V�B
WYB
WYB
X�B
XEB
X�B
X_B
X�B
X�B
YeB
Y�B
Y�B
Y�B
[�B
]�B
]�B
]IB
^5B
^B
]IB
^B
]�B
]�B
^OB
^�B
_B
_B
_VB
_�B
_�B
_VB
_�B
`'B
_�B
`B
_�B
_VB
_pB
_pB
_;B
_!B
_�B
_�B
`BB
`BB
`�B
`\B
`�B
`�B
`�B
abB
b�B
b�B
c B
cB
b�B
cTB
c:B
cTB
cnB
cnB
c�B
c�B
c�B
d&B
d�B
d�B
eB
d�B
d�B
e�B
e,B
e�B
e�B
fLB
f�B
f�B
f�B
fLB
f2B
e�B
ffB
f2B
f�B
f�B
g8B
h�B
h�B
h�B
iDB
i�B
i�B
iyB
i�B
jB
j�B
jB
j�B
j�B
k�B
k�B
kkB
l=B
lqB
k�B
lqB
l"B
m)B
m�B
mwB
m�B
nB
n�B
ncB
nIB
n}B
o5B
o5B
n�B
n�B
oB
o B
o�B
o�B
o�B
o�B
p�B
p�B
poB
p�B
q'B
p�B
qvB
q�B
q[B
q�B
rB
r�B
r�B
q�B
rB
r�B
r�B
s3B
s3B
s�B
s�B
s�B
tTB
tB
t9B
tB
t�B
tTB
t�B
uB
t�B
t�B
uB
t�B
u%B
t�B
t�B
u%B
t�B
u?B
uZB
u�B
uZB
u�B
u�B
u�B
v�B
vFB
v�B
vzB
v�B
v`B
v�B
wB
wfB
xB
xB
x8B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104916  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173653  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173654  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173654                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023701  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023701  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                