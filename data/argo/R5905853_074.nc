CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:35:56Z creation;2022-06-04T17:35:57Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173556  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               JA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�^E/�b�1   @�^E��O�@/��\)�c�V�u1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�33BǙ�B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�33B�ffB�  B���C  C  C  C  C
  C  C  C  C��C�3C  C  C  C�fC�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Q�@~�R@�(�@�\)A�A?�A_�A�A��
A��
A��
A���A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B��]B���B�B���B�(�BǏ]B�B���B�(�B���B�B���B���B���B���B���B�(�B�\)B���B��]C��C��C��C��C	��C��C��C��C�{C�C��C��C��C�GC�GC�GC!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>{C@{CA�GCC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\D�D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��3Aȹ�AȭCA�~�A�G�A�6FA�!�A�FA�PA�A�
rA�YA�;A�A��A��A� 4A��cA���A�A�YA�
�A��A�4A��A�!A�(XA�33A�8RA�B[A�MA�V�A�f�A�~(Aț�A��pA�+A�4A�A�#�A�(�A�YA���A�yrA�I�A�"4A�S�Aš�AÄ�A�IRA���A��A�K^A��XA��A�{�A��\A�:*A�t�A�ɺA�YA���A���A�E�A�1'A�IA���A���A�\]A�XyA{��AxTaAs%FAp>BAh��Af	lAdq�Ac�A\ZAWxlAS�AQ��AP��AO�NANԕAK!-AH��AFJ�AE@AB��A?  A<��A;�HA;A�A9��A7�0A7��A8�A8 �A7�A7}VA60�A5\)A4�A4͟A4v�A3~(A3@A2��A2�KA2ĜA2{JA2>BA1�A1W�A/�A/D�A.��A.M�A-��A-~�A-aA-/�A,ȴA,4nA+�OA+p;A+J#A*��A*-A)��A(ϫA'�0A'�A'&�A&��A&��A&�YA&�A&u�A%ںA$��A#��A#��A#{�A#P�A"�rA"]�A!�A!z�A!�A!A!{A!�A �]A ��A 33A��A�[A�A!-A�$A��A)�AE�AOA�A�An�A�A��A�PA?�A��A��A��A�hAm�AMjAA�,A�bA�fA(�A|�AA�/A��A�7A\)A�A�NA	�AE9A2aAYA�A?A�AxlAP�A�rA�hA@OA�WA��A�'A8�A��A��A�xA:�A�Aa|A�A
w�A
�A	�A	�'A	w�A	�A��A��A$A��A��AtTAN<AAe�A7AGA��A�aA�AAW�A?�A$A�A��A�=A4�A��A!�A Ov@�{J@�!�@�C�@�@�4@���@��@�Ɇ@�h
@���@���@�!�@��@�~�@�|�@�u�@��@�W?@�Ɇ@�@@�?�@�u�@쿱@��@���@��@��@�[�@�?}@�b@�kQ@��@�Dg@�֡@�b@��@��@��@�,�@�x@��@���@���@�=@�33@��M@�B�@��@�_p@��X@�u@��K@�n/@�@ؖ�@ئL@���@��)@�\)@��@���@�P�@�[W@�W?@�C-@ׅ@֗�@�YK@Ջ�@��@��@�`B@��@Ҿ�@�~�@�E�@��@ѣn@�Ta@���@�$t@�D�@ͿH@̼j@�S&@�b@��K@�S�@ȝI@ǿH@���@�.�@ľ�@ĆY@�I�@�	@��@�J�@��?@@°!@�:*@��n@�'�@�ں@�2�@���@��@�b�@�<6@���@�W�@�G@��z@��,@��@���@�_p@�4�@��@���@�� @���@�W�@�3�@�	@��0@���@��~@�=@��@��@���@�~�@��9@�:�@�@@��M@�d�@�@�_@��^@�@�n�@�?@�8�@�1'@��@��@��w@��s@��@��^@��@�'�@�͟@��R@�}V@�<�@���@�+@��s@��6@���@�i�@���@��@���@���@�D�@���@��Q@�w2@��|@��u@�4n@���@���@��@�G�@��y@��_@�~@��j@���@�*0@���@�V@��@��W@��H@�W?@��@��j@�,=@��j@���@��@�qv@�C@��@��@��8@��@��@�ȴ@��j@��@���@���@�{�@�W�@�.�@��T@�9�@��@��@��j@��O@�:*@��Z@��j@���@�_p@�@��B@��@���@���@��[@���@�K�@�+�@��s@��@�v�@�-�@��>@��@���@���@���@�D�@�!@���@��#@��@�j@�6z@�;@��@��@��?@���@��@��h@�Y�@��@��/@���@�<�@��@�^�@�*0@��@�'R@���@��n@�P�@�=�@�@��@�ȴ@���@���@�^5@�_@�Xy@�:*@�	@���@��@���@��@�|@�m]@��@��@���@�c�@�"h@���@�9�@��@��B@�U2@� �@�qv@�S�@�33@��@��	@��p@���@���@��@�U2@���@�@��{@�IR@�6z@�Y@���@���@�~(@�/�@�G@��>@��n@�0�@���@��x@�YK@��@��@�Z�@��@��F@�j@�'R@��:@��@�Ĝ@���@���@�l�@�`�@�U2@�J�@�GE@�=q@� �@���@���@���@�Vm@�+�@��@��@��@�V@�e@�@�v`@�=�@��@���@��`@�Ĝ@��@��@j�@]�@+@@~�m@~YK@~�@}�@}@|�5@|A�@{E9@{�@z�@z͟@z�}@zn�@z{@y��@yVm@x�5@x��@xPH@w�0@wl�@v��@v��@vkQ@v@�@u�^@t�`@tA�@t@s� @s��@s$t@r~�@r�@qX@qV@p�D@p,=@o�a@n�8@m�N@m�~@m0�@l�`@l��@lFt@k��@kdZ@j��@j��@j�@i�M@i�@h�@hq@h"h@g�g@g�{@g33@g�@f�X@f($@e�@eG�@d��@d(�@dG@co�@b��@bxl@bh
@b@�@aϫ@a2a@a+@a�@`�	@`D�@_�@_��@_�@_Mj@_"�@_
=@_ i@^��@^��@^�L@^xl@^Ov@^;�@^@]�Z@]�@]zx@]B�@]	l@["�@Z#:@Y�@Y��@Y��@Yf�@YY�@Y/@Y�@Y%@X�@X�.@X�@W�A@W��@W��@WA�@V��@V�@Ue,@T�U@T/�@S�[@SRT@S
=@R�1@RM�@Q��@Q�t@Q�@Qw2@QX@Q!�@P�@PM@P@P  @O�0@O��@O��@OZ�@N�m@N� @NV@N�@M��@M`B@L�@K�W@K�q@K�f@K\)@J~�@I��@I�M@H�|@H��@H@G��@G��@G�@F�@F��@F_@D�@C��@C"�@B�"@B�,@B��@BZ�@B�@A�@@Ɇ@@H@@�@?�@?�*@?l�@?�@>�s@>��@>~�@>z@>H�@>$�@=�@=x�@=�@<�j@<G@;;d@;�@:��@:��@:ں@:��@:s�@:H�@9�t@8��@8�@8c�@8Ft@8/�@8@7�g@7e�@6��@6_�@6.�@5�)@5��@5\�@5#�@4�@4�D@3��@3�4@3�@3{J@3x@3|�@3v`@3U�@3,�@2�8@2�6@2=q@1=�@0��@0Xy@0?�@0@/��@/g�@/6z@/�@.��@.Z�@-��@-��@-|@-m]@-#�@,��@,��@,��@,A�@+��@+��@+l�@*�c@*�r@)ϫ@)}�@)zx@)��@)�7@)�@)o @)G�@) \@(��@(��@(q@(2�@'�F@'|�@'H�@'Y@&��@&�F@&)�@%�@%�-@%o @%F@%%@$PH@$�@#�@#�&@#�}@#��@"�@"��@"��@!�Z@!��@!�@!��@!x�@!j@!*0@!@ ��@ ��@ ��@ ��@ �[@ �p@ �@ �p@ �)@ �)@ ��@ �U@ ��@ c�@ 9X@ (�@ !@   @�
@��@��@y�@b�@1�@�@ȴ@��@��@}V@l�@1�@-@�@��@A @�@�	@�f@�5@�@�@��@�I@��@�.@r�@7�@�@b@�@��@�F@��@o�@H�@�@��@u%@Q@6�@($@$�@��@�X@��@f�@L�@:�@�@�@l"@!@��@qv@�"@�s@�<@�@�r@�A@p;@H�@?@.�@�@�@��@�C@G�@%F@	l@�@tT@Ft@b@�r@�&@��@a@@�@H�@($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��3Aȹ�AȭCA�~�A�G�A�6FA�!�A�FA�PA�A�
rA�YA�;A�A��A��A� 4A��cA���A�A�YA�
�A��A�4A��A�!A�(XA�33A�8RA�B[A�MA�V�A�f�A�~(Aț�A��pA�+A�4A�A�#�A�(�A�YA���A�yrA�I�A�"4A�S�Aš�AÄ�A�IRA���A��A�K^A��XA��A�{�A��\A�:*A�t�A�ɺA�YA���A���A�E�A�1'A�IA���A���A�\]A�XyA{��AxTaAs%FAp>BAh��Af	lAdq�Ac�A\ZAWxlAS�AQ��AP��AO�NANԕAK!-AH��AFJ�AE@AB��A?  A<��A;�HA;A�A9��A7�0A7��A8�A8 �A7�A7}VA60�A5\)A4�A4͟A4v�A3~(A3@A2��A2�KA2ĜA2{JA2>BA1�A1W�A/�A/D�A.��A.M�A-��A-~�A-aA-/�A,ȴA,4nA+�OA+p;A+J#A*��A*-A)��A(ϫA'�0A'�A'&�A&��A&��A&�YA&�A&u�A%ںA$��A#��A#��A#{�A#P�A"�rA"]�A!�A!z�A!�A!A!{A!�A �]A ��A 33A��A�[A�A!-A�$A��A)�AE�AOA�A�An�A�A��A�PA?�A��A��A��A�hAm�AMjAA�,A�bA�fA(�A|�AA�/A��A�7A\)A�A�NA	�AE9A2aAYA�A?A�AxlAP�A�rA�hA@OA�WA��A�'A8�A��A��A�xA:�A�Aa|A�A
w�A
�A	�A	�'A	w�A	�A��A��A$A��A��AtTAN<AAe�A7AGA��A�aA�AAW�A?�A$A�A��A�=A4�A��A!�A Ov@�{J@�!�@�C�@�@�4@���@��@�Ɇ@�h
@���@���@�!�@��@�~�@�|�@�u�@��@�W?@�Ɇ@�@@�?�@�u�@쿱@��@���@��@��@�[�@�?}@�b@�kQ@��@�Dg@�֡@�b@��@��@��@�,�@�x@��@���@���@�=@�33@��M@�B�@��@�_p@��X@�u@��K@�n/@�@ؖ�@ئL@���@��)@�\)@��@���@�P�@�[W@�W?@�C-@ׅ@֗�@�YK@Ջ�@��@��@�`B@��@Ҿ�@�~�@�E�@��@ѣn@�Ta@���@�$t@�D�@ͿH@̼j@�S&@�b@��K@�S�@ȝI@ǿH@���@�.�@ľ�@ĆY@�I�@�	@��@�J�@��?@@°!@�:*@��n@�'�@�ں@�2�@���@��@�b�@�<6@���@�W�@�G@��z@��,@��@���@�_p@�4�@��@���@�� @���@�W�@�3�@�	@��0@���@��~@�=@��@��@���@�~�@��9@�:�@�@@��M@�d�@�@�_@��^@�@�n�@�?@�8�@�1'@��@��@��w@��s@��@��^@��@�'�@�͟@��R@�}V@�<�@���@�+@��s@��6@���@�i�@���@��@���@���@�D�@���@��Q@�w2@��|@��u@�4n@���@���@��@�G�@��y@��_@�~@��j@���@�*0@���@�V@��@��W@��H@�W?@��@��j@�,=@��j@���@��@�qv@�C@��@��@��8@��@��@�ȴ@��j@��@���@���@�{�@�W�@�.�@��T@�9�@��@��@��j@��O@�:*@��Z@��j@���@�_p@�@��B@��@���@���@��[@���@�K�@�+�@��s@��@�v�@�-�@��>@��@���@���@���@�D�@�!@���@��#@��@�j@�6z@�;@��@��@��?@���@��@��h@�Y�@��@��/@���@�<�@��@�^�@�*0@��@�'R@���@��n@�P�@�=�@�@��@�ȴ@���@���@�^5@�_@�Xy@�:*@�	@���@��@���@��@�|@�m]@��@��@���@�c�@�"h@���@�9�@��@��B@�U2@� �@�qv@�S�@�33@��@��	@��p@���@���@��@�U2@���@�@��{@�IR@�6z@�Y@���@���@�~(@�/�@�G@��>@��n@�0�@���@��x@�YK@��@��@�Z�@��@��F@�j@�'R@��:@��@�Ĝ@���@���@�l�@�`�@�U2@�J�@�GE@�=q@� �@���@���@���@�Vm@�+�@��@��@��@�V@�e@�@�v`@�=�@��@���@��`@�Ĝ@��@��@j�@]�@+@@~�m@~YK@~�@}�@}@|�5@|A�@{E9@{�@z�@z͟@z�}@zn�@z{@y��@yVm@x�5@x��@xPH@w�0@wl�@v��@v��@vkQ@v@�@u�^@t�`@tA�@t@s� @s��@s$t@r~�@r�@qX@qV@p�D@p,=@o�a@n�8@m�N@m�~@m0�@l�`@l��@lFt@k��@kdZ@j��@j��@j�@i�M@i�@h�@hq@h"h@g�g@g�{@g33@g�@f�X@f($@e�@eG�@d��@d(�@dG@co�@b��@bxl@bh
@b@�@aϫ@a2a@a+@a�@`�	@`D�@_�@_��@_�@_Mj@_"�@_
=@_ i@^��@^��@^�L@^xl@^Ov@^;�@^@]�Z@]�@]zx@]B�@]	l@["�@Z#:@Y�@Y��@Y��@Yf�@YY�@Y/@Y�@Y%@X�@X�.@X�@W�A@W��@W��@WA�@V��@V�@Ue,@T�U@T/�@S�[@SRT@S
=@R�1@RM�@Q��@Q�t@Q�@Qw2@QX@Q!�@P�@PM@P@P  @O�0@O��@O��@OZ�@N�m@N� @NV@N�@M��@M`B@L�@K�W@K�q@K�f@K\)@J~�@I��@I�M@H�|@H��@H@G��@G��@G�@F�@F��@F_@D�@C��@C"�@B�"@B�,@B��@BZ�@B�@A�@@Ɇ@@H@@�@?�@?�*@?l�@?�@>�s@>��@>~�@>z@>H�@>$�@=�@=x�@=�@<�j@<G@;;d@;�@:��@:��@:ں@:��@:s�@:H�@9�t@8��@8�@8c�@8Ft@8/�@8@7�g@7e�@6��@6_�@6.�@5�)@5��@5\�@5#�@4�@4�D@3��@3�4@3�@3{J@3x@3|�@3v`@3U�@3,�@2�8@2�6@2=q@1=�@0��@0Xy@0?�@0@/��@/g�@/6z@/�@.��@.Z�@-��@-��@-|@-m]@-#�@,��@,��@,��@,A�@+��@+��@+l�@*�c@*�r@)ϫ@)}�@)zx@)��@)�7@)�@)o @)G�@) \@(��@(��@(q@(2�@'�F@'|�@'H�@'Y@&��@&�F@&)�@%�@%�-@%o @%F@%%@$PH@$�@#�@#�&@#�}@#��@"�@"��@"��@!�Z@!��@!�@!��@!x�@!j@!*0@!@ ��@ ��@ ��@ ��@ �[@ �p@ �@ �p@ �)@ �)@ ��@ �U@ ��@ c�@ 9X@ (�@ !@   @�
@��@��@y�@b�@1�@�@ȴ@��@��@}V@l�@1�@-@�@��@A @�@�	@�f@�5@�@�@��@�I@��@�.@r�@7�@�@b@�@��@�F@��@o�@H�@�@��@u%@Q@6�@($@$�@��@�X@��@f�@L�@:�@�@�@l"@!@��@qv@�"@�s@�<@�@�r@�A@p;@H�@?@.�@�@�@��@�C@G�@%F@	l@�@tT@Ft@b@�r@�&@��@a@@�@H�@($11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BeBKBKB�BkB=BB�B�B�B;BVB�B \B �B �B!�B"hB#:B$�B&�B(�B)�B-�B0UB3�B7�B:DB>�BB�BESBK�BS�B^OBr�B��B�dB�oB��B��B�>BЗB�B	GB	S�B	��B
<�B
�B
�XB
��B�B
�}BJ�BUMB=�B1'B
�OB�Bg�BaBhB
��B
nB
DB
�B	��B	�;B	��B	ĜB	��B	��B	{�B	qAB	_pB	VB	O(B	FtB	0�B	*eB	 �B	sB	HB	
�B	�B�B�:B�OB��B�B��B�B�B�B�UB	
XB	�B	6`B	M�B	XB	\B	g8B	w�B	�~B	�B	�dB	��B	��B	�B	�jB	��B
%B
�B
(>B
/ B
6FB
6B
7�B
<B
CGB
E�B
F?B
G�B
J�B
P�B
UgB
VSB
V�B
Y1B
X�B
W$B
V�B
S�B
S�B
TB
S�B
S�B
SuB
SB
R:B
S�B
R:B
R�B
R:B
R�B
S�B
S@B
TaB
V9B
X�B
]IB
abB
e�B
h�B
i�B
i�B
hsB
f�B
dB
]dB
V�B
KxB
DMB
F�B
L�B
RoB
X�B
XyB
VB
U�B
W�B
W�B
W�B
W?B
W$B
W
B
WsB
W�B
X�B
XB
XyB
XB
VB
P�B
K�B
JXB
I�B
IRB
HfB
G�B
F�B
E9B
DgB
A B
A;B
@�B
@�B
@�B
?�B
?HB
>�B
?B
="B
<B
;�B
;B
:�B
9�B
7�B
6�B
6B
49B
4�B
4nB
3�B
4�B
3�B
3MB
2�B
1vB
/5B
-�B
.}B
./B
-B
+�B
*�B
*B
(�B
&fB
$@B
#�B
#B
"�B
"4B
!|B
!HB
!|B
!HB
�B
VB
]B
+B
�B	�PB	�?B	�B	�8B	�-B	��B	��B	߾B	�\B	��B	�BB	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�ZB	��B	� B	��B	�B	�hB	��B	�4B	�4B	�B	�B	�|B	�B	�-B	�B	�vB	��B	�pB	�!B	��B	߾B	ߤB	޸B	�/B	�7B	�SB	өB	ңB	��B	��B	�B	՛B	��B	�B	�}B	�oB	��B	�`B	��B
 �B
B
�B
�B
SB
[B
�B
[B
'B
 iB	��B
 4B	��B	�cB	�cB
  B
 �B
�B
YB
�B
�B
�B
�B
�B
�B
�B
 B
  B	�.B	�B	��B	�}B	��B	��B	�0B	�JB	�JB	�6B	�B	��B	��B	�<B	��B	��B	��B	��B	�B	�B	�<B	��B
 4B
 �B
 B
�B
�B
 B
�B
aB
�B
�B
B
�B
�B
gB
3B
3B
�B
9B
�B
�B
B
B
9B
mB
SB
B
mB
B
�B
gB
�B
�B
MB
�B
{B
GB
aB
-B
aB
-B
GB
mB
�B
B
?B
�B
�B
tB
�B
B
	RB
	B
	B
�B
	B
�B
	�B

rB

XB

�B
�B
�B
�B
PB
"B
�B
�B
VB
<B
�B
�B
B
�B
�B
PB
�B
�B
~B
�B
dB
B
B
DB

�B
)B
�B
�B
0B
dB
�B
�B
pB
B
BB
�B
�B
�B
B
{B
�B
�B
B
�B
�B
�B
$B
?B
sB
?B

B
YB
�B
�B
�B
�B

B
sB
EB
EB
+B
+B
_B
yB
yB
B
�B
�B
�B
�B
B
�B
B
eB
�B
�B
�B
�B
�B
7B
QB
kB
QB
QB
QB
kB
	B
WB
qB
�B
�B
�B
)B
]B
�B
]B
CB
]B
]B
�B
/B
dB
�B
~B
�B
�B
B
5B
�B
 BB
 'B
 'B
 �B
!�B
"NB
"NB
"NB
"4B
"�B
"�B
#:B
#�B
#�B
$�B
%`B
&B
%�B
$�B
$ZB
$ZB
$B
$�B
$�B
$�B
%`B
&B
($B
)�B
+6B
+B
*�B
*�B
*�B
*�B
+QB
+B
+kB
+�B
,�B
-)B
,�B
-�B
-�B
-�B
./B
.IB
.cB
.}B
/ B
/iB
/�B
/�B
0!B
1B
1�B
1�B
1�B
1�B
1�B
2B
2B
2B
2B
2-B
2-B
2aB
2�B
2�B
3B
3B
2�B
3B
3hB
3�B
3�B
4TB
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4TB
4�B
5�B
6`B
6�B
6�B
72B
7B
72B
7B
6�B
6�B
6�B
7B
7B
7B
7B
7B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
8B
8B
7�B
88B
8�B
9$B
9	B
9	B
8�B
8�B
9�B
9�B
9�B
9�B
:*B
:DB
:^B
:�B
;�B
;�B
<6B
<PB
<PB
<�B
="B
=qB
>B
=�B
>�B
>�B
?cB
?�B
?�B
?�B
@B
@4B
@4B
@4B
@B
@OB
@OB
@�B
AB
A;B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
A;B
AUB
A B
@�B
AB
A B
AB
AB
AUB
A�B
AoB
AoB
A�B
A�B
A�B
A�B
BB
BB
B'B
B'B
B'B
B�B
B�B
BAB
D�B
E�B
FB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
G�B
G�B
G�B
G�B
HfB
H�B
IB
IlB
I�B
J#B
JXB
J�B
J�B
KB
KDB
K�B
K�B
K�B
K�B
K�B
LB
L~B
LJB
L0B
L0B
L0B
LB
K�B
LB
LB
LJB
LJB
LdB
LdB
LdB
L�B
MjB
M�B
M�B
MB
NB
N"B
N�B
N�B
O(B
O�B
OvB
O�B
PbB
PB
P.B
O�B
RB
RoB
SB
S@B
SuB
S�B
TB
TaB
T�B
VB
V�B
W$B
W
B
WsB
W�B
XB
XEB
X_B
XyB
X+B
X�B
X�B
X�B
YB
X�B
YKB
Y�B
Z7B
Y�B
YB
YB
YB
YB
YKB
YeB
Y�B
YeB
Y�B
YB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
[	B
[WB
[�B
[�B
\B
\]B
\�B
]~B
]�B
]~B
]�B
]�B
]~B
]�B
]�B
]�B
]�B
^B
^jB
_!B
_pB
`B
_�B
`BB
`BB
`�B
`�B
`�B
`�B
abB
a|B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
c B
cnB
cnB
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dtB
dtB
d�B
d�B
eB
e�B
fLB
f�B
gB
gmB
g�B
hXB
h�B
h�B
i*B
i*B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jeB
jB
jB
kQB
k6B
k6B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mB
mCB
m�B
nB
nB
m�B
nB
n/B
ncB
n�B
n�B
n�B
o B
o B
oOB
oOB
oiB
oiB
o�B
o�B
oOB
oOB
oOB
pB
p;B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
qAB
q[B
q[B
qvB
q�B
r-B
r-B
rGB
r|B
shB
sMB
s�B
s�B
s�B
s�B
tB
tB
tTB
tTB
t�B
t�B
t�B
uB
uZB
u�B
u�B
v`B
v�B
v�B
w2B
wfB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
y�B
z*B
z*B
z�B
z�B
{JB
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BeBKBKB�BkB=BB�B�B�B;BVB�B \B �B �B!�B"hB#:B$�B&�B(�B)�B-�B0UB3�B7�B:DB>�BB�BESBK�BS�B^OBr�B��B�dB�oB��B��B�>BЗB�B	GB	S�B	��B
<�B
�B
�XB
��B�B
�}BJ�BUMB=�B1'B
�OB�Bg�BaBhB
��B
nB
DB
�B	��B	�;B	��B	ĜB	��B	��B	{�B	qAB	_pB	VB	O(B	FtB	0�B	*eB	 �B	sB	HB	
�B	�B�B�:B�OB��B�B��B�B�B�B�UB	
XB	�B	6`B	M�B	XB	\B	g8B	w�B	�~B	�B	�dB	��B	��B	�B	�jB	��B
%B
�B
(>B
/ B
6FB
6B
7�B
<B
CGB
E�B
F?B
G�B
J�B
P�B
UgB
VSB
V�B
Y1B
X�B
W$B
V�B
S�B
S�B
TB
S�B
S�B
SuB
SB
R:B
S�B
R:B
R�B
R:B
R�B
S�B
S@B
TaB
V9B
X�B
]IB
abB
e�B
h�B
i�B
i�B
hsB
f�B
dB
]dB
V�B
KxB
DMB
F�B
L�B
RoB
X�B
XyB
VB
U�B
W�B
W�B
W�B
W?B
W$B
W
B
WsB
W�B
X�B
XB
XyB
XB
VB
P�B
K�B
JXB
I�B
IRB
HfB
G�B
F�B
E9B
DgB
A B
A;B
@�B
@�B
@�B
?�B
?HB
>�B
?B
="B
<B
;�B
;B
:�B
9�B
7�B
6�B
6B
49B
4�B
4nB
3�B
4�B
3�B
3MB
2�B
1vB
/5B
-�B
.}B
./B
-B
+�B
*�B
*B
(�B
&fB
$@B
#�B
#B
"�B
"4B
!|B
!HB
!|B
!HB
�B
VB
]B
+B
�B	�PB	�?B	�B	�8B	�-B	��B	��B	߾B	�\B	��B	�BB	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�ZB	��B	� B	��B	�B	�hB	��B	�4B	�4B	�B	�B	�|B	�B	�-B	�B	�vB	��B	�pB	�!B	��B	߾B	ߤB	޸B	�/B	�7B	�SB	өB	ңB	��B	��B	�B	՛B	��B	�B	�}B	�oB	��B	�`B	��B
 �B
B
�B
�B
SB
[B
�B
[B
'B
 iB	��B
 4B	��B	�cB	�cB
  B
 �B
�B
YB
�B
�B
�B
�B
�B
�B
�B
 B
  B	�.B	�B	��B	�}B	��B	��B	�0B	�JB	�JB	�6B	�B	��B	��B	�<B	��B	��B	��B	��B	�B	�B	�<B	��B
 4B
 �B
 B
�B
�B
 B
�B
aB
�B
�B
B
�B
�B
gB
3B
3B
�B
9B
�B
�B
B
B
9B
mB
SB
B
mB
B
�B
gB
�B
�B
MB
�B
{B
GB
aB
-B
aB
-B
GB
mB
�B
B
?B
�B
�B
tB
�B
B
	RB
	B
	B
�B
	B
�B
	�B

rB

XB

�B
�B
�B
�B
PB
"B
�B
�B
VB
<B
�B
�B
B
�B
�B
PB
�B
�B
~B
�B
dB
B
B
DB

�B
)B
�B
�B
0B
dB
�B
�B
pB
B
BB
�B
�B
�B
B
{B
�B
�B
B
�B
�B
�B
$B
?B
sB
?B

B
YB
�B
�B
�B
�B

B
sB
EB
EB
+B
+B
_B
yB
yB
B
�B
�B
�B
�B
B
�B
B
eB
�B
�B
�B
�B
�B
7B
QB
kB
QB
QB
QB
kB
	B
WB
qB
�B
�B
�B
)B
]B
�B
]B
CB
]B
]B
�B
/B
dB
�B
~B
�B
�B
B
5B
�B
 BB
 'B
 'B
 �B
!�B
"NB
"NB
"NB
"4B
"�B
"�B
#:B
#�B
#�B
$�B
%`B
&B
%�B
$�B
$ZB
$ZB
$B
$�B
$�B
$�B
%`B
&B
($B
)�B
+6B
+B
*�B
*�B
*�B
*�B
+QB
+B
+kB
+�B
,�B
-)B
,�B
-�B
-�B
-�B
./B
.IB
.cB
.}B
/ B
/iB
/�B
/�B
0!B
1B
1�B
1�B
1�B
1�B
1�B
2B
2B
2B
2B
2-B
2-B
2aB
2�B
2�B
3B
3B
2�B
3B
3hB
3�B
3�B
4TB
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4TB
4�B
5�B
6`B
6�B
6�B
72B
7B
72B
7B
6�B
6�B
6�B
7B
7B
7B
7B
7B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
8B
8B
7�B
88B
8�B
9$B
9	B
9	B
8�B
8�B
9�B
9�B
9�B
9�B
:*B
:DB
:^B
:�B
;�B
;�B
<6B
<PB
<PB
<�B
="B
=qB
>B
=�B
>�B
>�B
?cB
?�B
?�B
?�B
@B
@4B
@4B
@4B
@B
@OB
@OB
@�B
AB
A;B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
A;B
AUB
A B
@�B
AB
A B
AB
AB
AUB
A�B
AoB
AoB
A�B
A�B
A�B
A�B
BB
BB
B'B
B'B
B'B
B�B
B�B
BAB
D�B
E�B
FB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
G�B
G�B
G�B
G�B
HfB
H�B
IB
IlB
I�B
J#B
JXB
J�B
J�B
KB
KDB
K�B
K�B
K�B
K�B
K�B
LB
L~B
LJB
L0B
L0B
L0B
LB
K�B
LB
LB
LJB
LJB
LdB
LdB
LdB
L�B
MjB
M�B
M�B
MB
NB
N"B
N�B
N�B
O(B
O�B
OvB
O�B
PbB
PB
P.B
O�B
RB
RoB
SB
S@B
SuB
S�B
TB
TaB
T�B
VB
V�B
W$B
W
B
WsB
W�B
XB
XEB
X_B
XyB
X+B
X�B
X�B
X�B
YB
X�B
YKB
Y�B
Z7B
Y�B
YB
YB
YB
YB
YKB
YeB
Y�B
YeB
Y�B
YB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
[	B
[WB
[�B
[�B
\B
\]B
\�B
]~B
]�B
]~B
]�B
]�B
]~B
]�B
]�B
]�B
]�B
^B
^jB
_!B
_pB
`B
_�B
`BB
`BB
`�B
`�B
`�B
`�B
abB
a|B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
c B
cnB
cnB
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dtB
dtB
d�B
d�B
eB
e�B
fLB
f�B
gB
gmB
g�B
hXB
h�B
h�B
i*B
i*B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jeB
jB
jB
kQB
k6B
k6B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mB
mCB
m�B
nB
nB
m�B
nB
n/B
ncB
n�B
n�B
n�B
o B
o B
oOB
oOB
oiB
oiB
o�B
o�B
oOB
oOB
oOB
pB
p;B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
qAB
q[B
q[B
qvB
q�B
r-B
r-B
rGB
r|B
shB
sMB
s�B
s�B
s�B
s�B
tB
tB
tTB
tTB
t�B
t�B
t�B
uB
uZB
u�B
u�B
v`B
v�B
v�B
w2B
wfB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
y�B
z*B
z*B
z�B
z�B
{JB
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104914  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173556  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173557  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173557                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023605  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023605  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                