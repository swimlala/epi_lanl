CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-28T21:35:08Z creation;2018-05-28T21:35:14Z conversion to V3.1;2019-12-19T07:37:14Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   PRES_ADJUSTED_QC         
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
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180528213508  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_245                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�f2e�8 1   @�f31M��@4DɅ�oi�dN�IQ��1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D�D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��D�/\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�/A�1'A�1'A�1'A�1'A�33A�33A�33A�33A�33A�5?A�33A�5?A�5?A�7LA�7LA�5?A�33A��
A�JA�Q�A�bNA���A�7LAĝ�A���A�hsA���A+A���A�S�A��A�
=A�;dA��A�1A�C�A�E�A��/A�|�A�O�A�VA�\)A��DA��-A�C�A�/A��A�9XA��yA��FA�M�A���A�A�A��A�l�A�Q�A�;dA��/A��+A��A�XA�A�  A��;A�r�A���A��+A��A���A�^5A��jA�=qA��9A��A��#A�A��\A��uA�
=A���A���A�x�A��hA�7LA�\)A�  A��\A�=qA�ȴA���A�ƨA��uA�O�A�A�A��A�5?A�p�A��A��jA�jA�A�|�A~��A~$�A{�TAyx�Av�As\)Aq�
Aq�Ap �Al�RAk�AjVAf�9Ad�\Ac�Aa%A`�/A^��A[AZv�AY�PAX5?AW��AWK�AUVASdZASG�AR��APbAM?}AK|�AJAI7LAHv�AHAGXAE��ADA�AC%ABA@��A@bA?�A<�HA:��A7`BA4��A3��A2�A2E�A1�A//A-�A-��A-G�A+�A*jA(A&ffA%��A%&�A#�A"��A"JA!�^A ��A?}A�9A=qA�DA�wA�A5?A��A�AXA�-AXA�A$�A��A;dA�A��A;dAƨA�/A`BA
��A
n�A��AhsA��A��A�hAjA�hA�A��AA�AA�-A%@��
@�33@���@�G�@�+@���@�^5@��9@�S�@�v�@��@�p�@�r�@�@��#@�%@�Z@�C�@���@�1@睲@��y@�=q@�x�@��@�M�@߮@ޏ\@�E�@��#@��@��;@ڗ�@���@׍P@�+@�@ָR@�n�@��@ա�@ԋD@�1'@ӥ�@���@�(�@ύP@��@̼j@��@�$�@�G�@��@�C�@�n�@�G�@ċD@�I�@��@�V@���@�&�@�j@��@�5?@�@�O�@��@�j@���@���@�@�hs@���@� �@�S�@�~�@�@���@��@�j@��@��@��^@�p�@���@�Q�@�(�@�b@��w@��@�\)@�K�@�
=@�V@��^@��@���@��
@�;d@��R@�=q@�@�p�@��@���@��@� �@�|�@��@��H@�
=@�+@�
=@��R@��@�/@�Ĝ@���@�Q�@��
@�|�@�K�@�C�@�33@�o@���@��+@�n�@�^5@��#@���@��7@�?}@���@��/@���@�Q�@�9X@�(�@��m@�\)@���@�ȴ@���@�n�@�E�@�5?@��@��h@�x�@��@���@��-@���@���@�V@���@���@���@�Ĝ@��@��u@���@���@��u@�j@���@���@�~�@�=q@�J@�@�7L@���@��9@�z�@� �@��
@�S�@��@��@��R@���@�v�@�-@��@�@�hs@�V@��D@�ƨ@��P@�K�@��@�C�@�\)@��R@�n�@�ff@�n�@�^5@�V@�$�@���@�hs@�?}@�V@��@���@��@��u@�9X@�b@��m@��w@��@���@�t�@�\)@�S�@�"�@�
=@��@��y@�ȴ@�v�@�V@�$�@���@�`B@�V@��`@���@��9@���@��@�r�@�bN@�Q�@�A�@�b@���@�t�@�l�@�K�@�C�@�33@��@��+@�^5@�=q@�{@�J@��@��@��T@���@�@���@��7@�x�@�%@��u@�j@�A�@�1@��@�dZ@�"�@��@���@�M�@�=q@��T@�@���@�V@��`@��`@��`@���@��@�r�@�b@��
@���@�K�@��@��@�o@�@��y@��@���@��\@�~�@�^5@�{@�@���@��T@��^@���@�p�@�O�@�G�@�?}@�/@�&�@��`@���@��u@��u@��u@��@���@��@�j@�(�@�  @�w@;d@
=@~��@}�T@|�@|��@|��@|(�@{��@{dZ@{"�@{@z�!@z-@y�#@y��@y��@y��@y�7@yX@y&�@x�`@x�@xbN@x �@w�@w|�@w
=@vff@u�@u��@u@u�-@u�@up�@u`B@u/@t�j@t��@s��@sS�@s"�@r�@r�H@r��@qhs@q�@p�9@pQ�@o�w@ol�@n�R@n5?@m@lj@l(�@k��@k�F@k�@k33@j��@j�\@jJ@i�@hĜ@h �@g�;@g�@gl�@g;d@f�R@e�h@e?}@d��@dZ@d9X@cƨ@cC�@b�@b��@b�@a��@a7L@a�@`��@`Ĝ@`�u@`bN@`Q�@_�@_\)@_�@^�@^ff@]��@]`B@\�@\z�@[�F@["�@Y��@Y��@Y��@Y�7@Yhs@Y7L@X��@X �@W�;@W�@WK�@V�y@V�R@Vv�@V$�@U�h@UO�@T��@TZ@S�m@S��@S33@R��@R�!@R��@R�\@R^5@Q�@Q�@P��@PbN@P1'@P �@Pb@O�@O�@N�R@Nv�@Nff@NE�@N5?@N5?@N@M��@M��@M�@M/@LZ@L�@K�
@K�F@K��@Kt�@Ko@J��@J~�@J-@I��@I�7@H�`@HbN@G�w@G|�@G\)@G;d@G�@Fȴ@F�+@Fv�@FV@F{@E�@E@E�@EV@D�@D�D@DZ@C��@Ct�@CS�@C"�@C@B��@B^5@A��@A��@A��@AG�@@�u@@A�@@  @?|�@?K�@?K�@?�@>ȴ@>v�@>E�@>@=?}@<�/@<�@<z�@<I�@<�@;��@;�m@;ƨ@;�F@;��@;dZ@;C�@;@:��@:^5@9��@9�7@9X@9&�@8�`@8��@8bN@8b@7��@7|�@6��@6�@6��@6ff@65?@6@5�@5�@4��@4��@4��@4z�@4Z@49X@3��@3t�@3"�@2��@2�\@2^5@2J@1��@1X@0�`@0��@0bN@0b@/�;@/|�@/
=@.��@.��@.�R@.��@.V@-��@-�h@-`B@,�@,�@,�D@,z�@,9X@+��@+�F@+t�@*�@*��@*^5@*-@)��@)�@)�#@)x�@)X@)7L@)&�@(�`@(Q�@'�;@'\)@'+@'+@&��@&�@&ȴ@&��@&V@&V@&$�@&@%�@%�T@%�-@%p�@%/@$��@$��@#��@#�
@#�
@#�
@#�
@#��@#�@#dZ@#33@#@"�H@"��@"�!@"�\@"�\@"~�@"n�@"-@"J@!�#@!�7@!7L@ ��@ ��@ r�@ Q�@�@�w@�P@l�@K�@+@�@�@
=@�@ȴ@�R@��@v�@E�@$�@�@��@�-@��@�h@�@O�@V@V@�/@�j@��@j@I�@(�@��@ƨ@�F@��@�@�@t�@C�@�H@�!@�\@~�@n�@^5@-@�@�@�#@�#@�^@�7@G�@&�@Ĝ@�@bN@Q�@bN@Q�@b@  @�;@��@�w@�@|�@+@��@��@V@{@�@��@�-@��@��@�h@O�@V@�D@Z@9X@�@��@�
@��@"�@�H@��@��@��@^5@=q@-@�@J@��@��@��@��@�7@hs@%@�`@�`@��@��@��@Q�@1'@b@�@��@�P@�P@|�@K�@
=@��@�@�+@E�@{@�T@�-@O�@/@/@��@�j@z�@9X@�@��@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�/A�1'A�1'A�1'A�1'A�33A�33A�33A�33A�33A�5?A�33A�5?A�5?A�7LA�7LA�5?A�33A��
A�JA�Q�A�bNA���A�7LAĝ�A���A�hsA���A+A���A�S�A��A�
=A�;dA��A�1A�C�A�E�A��/A�|�A�O�A�VA�\)A��DA��-A�C�A�/A��A�9XA��yA��FA�M�A���A�A�A��A�l�A�Q�A�;dA��/A��+A��A�XA�A�  A��;A�r�A���A��+A��A���A�^5A��jA�=qA��9A��A��#A�A��\A��uA�
=A���A���A�x�A��hA�7LA�\)A�  A��\A�=qA�ȴA���A�ƨA��uA�O�A�A�A��A�5?A�p�A��A��jA�jA�A�|�A~��A~$�A{�TAyx�Av�As\)Aq�
Aq�Ap �Al�RAk�AjVAf�9Ad�\Ac�Aa%A`�/A^��A[AZv�AY�PAX5?AW��AWK�AUVASdZASG�AR��APbAM?}AK|�AJAI7LAHv�AHAGXAE��ADA�AC%ABA@��A@bA?�A<�HA:��A7`BA4��A3��A2�A2E�A1�A//A-�A-��A-G�A+�A*jA(A&ffA%��A%&�A#�A"��A"JA!�^A ��A?}A�9A=qA�DA�wA�A5?A��A�AXA�-AXA�A$�A��A;dA�A��A;dAƨA�/A`BA
��A
n�A��AhsA��A��A�hAjA�hA�A��AA�AA�-A%@��
@�33@���@�G�@�+@���@�^5@��9@�S�@�v�@��@�p�@�r�@�@��#@�%@�Z@�C�@���@�1@睲@��y@�=q@�x�@��@�M�@߮@ޏ\@�E�@��#@��@��;@ڗ�@���@׍P@�+@�@ָR@�n�@��@ա�@ԋD@�1'@ӥ�@���@�(�@ύP@��@̼j@��@�$�@�G�@��@�C�@�n�@�G�@ċD@�I�@��@�V@���@�&�@�j@��@�5?@�@�O�@��@�j@���@���@�@�hs@���@� �@�S�@�~�@�@���@��@�j@��@��@��^@�p�@���@�Q�@�(�@�b@��w@��@�\)@�K�@�
=@�V@��^@��@���@��
@�;d@��R@�=q@�@�p�@��@���@��@� �@�|�@��@��H@�
=@�+@�
=@��R@��@�/@�Ĝ@���@�Q�@��
@�|�@�K�@�C�@�33@�o@���@��+@�n�@�^5@��#@���@��7@�?}@���@��/@���@�Q�@�9X@�(�@��m@�\)@���@�ȴ@���@�n�@�E�@�5?@��@��h@�x�@��@���@��-@���@���@�V@���@���@���@�Ĝ@��@��u@���@���@��u@�j@���@���@�~�@�=q@�J@�@�7L@���@��9@�z�@� �@��
@�S�@��@��@��R@���@�v�@�-@��@�@�hs@�V@��D@�ƨ@��P@�K�@��@�C�@�\)@��R@�n�@�ff@�n�@�^5@�V@�$�@���@�hs@�?}@�V@��@���@��@��u@�9X@�b@��m@��w@��@���@�t�@�\)@�S�@�"�@�
=@��@��y@�ȴ@�v�@�V@�$�@���@�`B@�V@��`@���@��9@���@��@�r�@�bN@�Q�@�A�@�b@���@�t�@�l�@�K�@�C�@�33@��@��+@�^5@�=q@�{@�J@��@��@��T@���@�@���@��7@�x�@�%@��u@�j@�A�@�1@��@�dZ@�"�@��@���@�M�@�=q@��T@�@���@�V@��`@��`@��`@���@��@�r�@�b@��
@���@�K�@��@��@�o@�@��y@��@���@��\@�~�@�^5@�{@�@���@��T@��^@���@�p�@�O�@�G�@�?}@�/@�&�@��`@���@��u@��u@��u@��G�O�G�O�@�j@�(�@�  @�w@;d@
=@~��@}�T@|�@|��@|��@|(�@{��@{dZ@{"�@{@z�!@z-@y�#@y��@y��@y��@y�7@yX@y&�@x�`@x�@xbN@x �@w�@w|�@w
=@vff@u�@u��@u@u�-@u�@up�@u`B@u/@t�j@t��@s��@sS�@s"�@r�@r�HG�O�@qhs@q�@p�9@pQ�@o�w@ol�@n�R@n5?@m@lj@l(�@k��@k�F@k�@k33@j��@j�\@jJ@i�@hĜ@h �@g�;@g�@gl�@g;d@f�R@e�h@e?}@d��@dZ@d9X@cƨ@cC�@b�@b��@b�@a��@a7L@a�@`��@`Ĝ@`�u@`bN@`Q�@_�@_\)@_�@^�@^ff@]��@]`B@\�@\z�@[�F@["�@Y��@Y��@Y��@Y�7@Yhs@Y7L@X��@X �@W�;@W�@WK�@V�y@V�R@Vv�@V$�@U�h@UO�@T��@TZ@S�m@S��@S33@R��@R�!@R��@R�\G�O�@Q�@Q�@P��@PbN@P1'@P �@Pb@O�@O�@N�R@Nv�@Nff@NE�@N5?@N5?@N@M��@M��@M�@M/@LZ@L�@K�
@K�F@K��@Kt�@Ko@J��@J~�@J-@I��@I�7@H�`@HbN@G�w@G|�@G\)@G;d@G�@Fȴ@F�+@Fv�@FV@F{@E�@E@E�@EV@D�@D�D@DZ@C��@Ct�@CS�@C"�@C@B��@B^5@A��@A��@A��@AG�@@�u@@A�@@  @?|�@?K�@?K�@?�@>ȴ@>v�@>E�@>@=?}@<�/@<�@<z�@<I�@<�@;��@;�m@;ƨ@;�F@;��@;dZ@;C�@;@:��@:^5@9��@9�7@9X@9&�@8�`@8��@8bN@8b@7��@7|�@6��@6�@6��@6ff@65?@6@5�@5�@4��@4��@4��@4z�@4Z@49X@3��@3t�@3"�@2��@2�\@2^5@2J@1��@1X@0�`@0��@0bN@0b@/�;@/|�@/
=@.��@.��@.�R@.��@.V@-��@-�h@-`B@,�@,�@,�D@,z�@,9X@+��@+�F@+t�@*�@*��@*^5@*-@)��@)�@)�#@)x�@)X@)7L@)&�@(�`@(Q�@'�;@'\)@'+@'+@&��@&�@&ȴ@&��@&V@&V@&$�@&@%�@%�T@%�-@%p�@%/@$��@$��@#��@#�
@#�
@#�
@#�
@#��@#�@#dZ@#33@#@"�H@"��@"�!@"�\@"�\@"~�@"n�@"-@"J@!�#@!�7@!7L@ ��@ ��@ r�@ Q�@�@�w@�P@l�@K�@+@�@�@
=@�@ȴ@�R@��@v�@E�@$�@�@��@�-@��@�h@�@O�@V@V@�/@�j@��@j@I�@(�@��@ƨ@�F@��@�@�@t�@C�@�H@�!@�\@~�@n�@^5@-@�@�@�#@�#@�^@�7@G�@&�@Ĝ@�@bN@Q�@bN@Q�@b@  @�;@��@�w@�@|�@+@��@��@V@{@�@��@�-@��@��@�h@O�@V@�D@Z@9X@�@��@�
@��@"�@�H@��@��@��@^5@=q@-@�@J@��@��@��@��@�7@hs@%@�`@�`@��@��@��@Q�@1'@b@�@��@�P@�P@|�@K�@
=@��@�@�+@E�@{@�T@�-@O�@/@/@��@�j@z�@9X@�@��@ƨ111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
 �B
�B
�B
hB
oB
u�B/Bz�B�%B��B��B�XB��B�B�BBVB%�B2-B:^BB�BR�BS�BVBw�B� B|�By�Bw�B�B�1B�uB��B�PB�PB�\B��B�uB�1Bx�Be`Bq�BjBT�BD�BG�BN�BA�B:^B0!B&�B�B�B�B�B�)B�-B��B��B�^B��B�{B�Bs�Bk�BZBH�B1'BB
��B
�B
�%B
�B
� B
q�B
aHB
E�B
XB
VB
T�B
K�B
5?B
$�B
&�B
2-B
49B
8RB
)�B
%�B
�B
VB	��B	�mB	��B	ǮB	��B	��B	��B	��B	��B	�B	q�B	u�B	jB	iyB	ZB	<jB	C�B	@�B	49B	49B	-B	�B	�B	'�B	"�B	DB��B��B��B��B��B��B�B�NB�;B�/B�/B�B�B�BŢB�RB��B��B��B��B��B��B�\B��B��B��B�=B� Bv�Bz�B�B� Bx�Bx�Bx�B{�Br�Bl�Bq�Bq�BdZBl�Bl�BhsB_;Bk�BiyBcTBp�Bq�Bm�Be`Be`BhsBq�Bm�BaHB`BB_;BgmBffB]/B\)BjBdZBo�Bk�Bk�Bp�Bo�Bq�Bq�Bo�BjBdZBl�BiyBcTB\)BVB`BB`BBbNBffBgmBgmBdZBaHBcTBffBe`B`BB\)Be`BiyBffBdZBbNB]/B]/BYBe`Bm�Bk�BjBhsBiyBl�Bs�B|�B}�B|�B|�B{�Bx�Bv�By�Bw�Br�Bs�B~�B}�Bx�B� B�+B�DB�DB�oB�uB�{B��B��B��B��B��B��B��B��B�B�-B�'B�3B�qB�qBB��BŢBɺB��B��B��B�B�)B�)B�5B�HB�ZB�sB��B��B��B	B	B	B	1B	
=B	DB	DB		7B	VB	hB	�B	�B	�B	$�B	'�B	+B	/B	0!B	33B	33B	7LB	8RB	>wB	C�B	G�B	L�B	L�B	K�B	K�B	M�B	W
B	\)B	^5B	^5B	bNB	gmB	k�B	l�B	l�B	l�B	p�B	s�B	s�B	r�B	t�B	w�B	w�B	z�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�7B	�DB	�=B	�JB	�hB	�uB	�uB	�uB	�{B	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�FB	�^B	�dB	�jB	�jB	�qB	�}B	�}B	�}B	��B	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�)B	�5B	�;B	�BB	�HB	�NB	�HB	�TB	�TB	�TB	�`B	�fB	�fB	�fB	�`B	�mB	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
1B
1B
+B
%B
%B
+B
	7B
DB

=B
JB
VB
VB
PB
PB
PB
PB
VB
VB
PB
JB
VB
VB
VB
PB
VB
PB
VB
\B
\B
VB
VB
PB
PB
bB
hB
hB
oB
uB
bB
VB
VB
\B
\B
VB
\B
VB
PB
PB
oB
oB
hB
hB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
"�B
!�B
!�B
 �B
!�B
!�B
 �B
#�B
#�B
#�B
"�B
 �B
#�B
$�B
#�B
#�B
$�B
#�B
#�B
#�B
!�B
'�B
(�B
(�B
(�B
)�B
(�B
)�B
(�B
(�B
+B
+B
,B
,B
,B
,B
+B
(�B
,B
,B
,B
-B
-B
-B
.B
.B
-B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
/B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
1'B
2-B
1'B
5?B
6FB
6FB
6FB
5?B
49B
49B
6FB
6FB
6FB
6FB
8RB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
9XB
9XB
<jB
;dB
=qB
>wB
=qB
=qB
<jB
;dB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
=qB
=qB
?}B
?}B
@�B
@�B
@�B
?}B
>wB
?}B
?}B
?}B
?}B
>wB
?}B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
C�B
E�B
F�B
E�B
G�B
G�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
I�B
J�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
T�B
VB
W
B
VB
W
B
W
B
W
B
YB
YB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
YB
YB
YB
YB
YB
ZB
ZB
[#B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
[#B
ZB
[#B
\)B
^5B
_;B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
_;B
_;B
^5B
^5B
_;B
_;B
aHB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
aHB
bNB
bNB
bNB
aHB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
m�B
m�B
o�B
p�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
x�B
x�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
 �B
�B
#B
[B

B
z�B/�B{JB��B�B�B�^B��B�B�nB�B�B'�B4TB=BD�BT�BV9BX�BxRB�iB}�B{By�B��B�B��B�9B��B�B��B��B��B�	B|Bg�BrBkkBXBGzBI�BPBD�B<�B2�B(�BqB��B�B�BߊB��B�B��B�B�nB��B�Bu�BmB\]BJ=B4nB�B
�MB
��B
��B
�3B
�B
sMB
d@B
IB
X�B
V�B
UgB
MB
9	B
&�B
'�B
2�B
5ZB
9�B
-]B
($B
!HB
NB	��B	�B	ңB	�lB	οB	ðB	��B	�kB	�VB	�%B	tnB	w�B	l�B	jeB	\�B	@4B	EB	A�B	5�B	4�B	./B	OB	_B	(XB	$@B	�B	;B	 B��B��B��B��B��B�B��B��BޞB��B�1B��B��B�B�CB��B�B�8B��B�@B�:B��B�)B�sB�dB�[By�B|�B��B� BzxBzDBy�B|�BtTBnIBr�Br�BffBm�Bm�Bi�BaHBlqBj�BezBq'BraBoBgmBgmBjBraBn}BcnBa�BaBhXBgmB_VB^BkkBe�Bp;BmBl�BqvBpUBr-Br-BpUBk�Be�BmBj0Bd�B]�BX+BaHBabBc:BgBg�Bg�Be,BbNBd&BgBfBaHB]�Be�Bi�BgBd�Bc B^jB^�BZ�BfBm�BlBkQBiyBjBm�Bt�B}<B~(B}<B}<B|PByXBw�BzDBx�BtBt�B}B~�Bz^B� B��B��B�0B�B�,B�MB�CB�B��B�VB�NB�ZB��B��B�wB�|B��B��B��B�B�-B�AB�%B�=B�pB�}BԕBچBܒBܬB��B��B�B�*B�B�RB�<B	-B	SB	�B	�B	
rB	xB	�B		�B	�B	�B	B	7B	 BB	%FB	(XB	+kB	/OB	0oB	3hB	3�B	7�B	8�B	>�B	C�B	G�B	L�B	MB	L0B	LJB	NpB	WYB	\]B	^�B	^�B	b�B	g�B	k�B	l�B	l�B	l�B	p�B	s�B	s�B	sB	t�B	xB	xB	{B	.B	HB	�OB	�AB	�-B	�oB	�uB	�gB	�_B	�KB	�fB	�lB	�^B	��B	��B	��B	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�bB	�BB	�B	�B	�B	�8B	�mB	�=B	�IB	�OB	�iB	�|B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�NB	� B	�B	��B	�B	�B	�,B	�[B	�FB	�+B	�KB	�7B	�=B	�CB	�IB	�xB	�OB	�VB	�\B	�bB	�hB	�|B	�nB	�nB	�B	�zB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�$B	�*B	�B	�B	�B	�JB	�B	�(B	�.B
 OB
 4B
'B
UB
-B
GB
oB
?B
1B
1B
EB
YB
tB
�B
	lB
xB

rB
dB
VB
pB
�B
jB
jB
�B
pB
pB
jB
~B
pB
�B
pB
�B
pB
jB
pB
\B
\B
pB
pB
�B
�B
bB
hB
hB
oB
�G�O�G�O�B
�B
vB
vB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
"�B
!�B
!�B
 �B
!�B
!�B
 �B
#�B
#�B
#�B
# G�O�B
$B
$�B
$B
$&B
%B
$&B
$B
$B
"4B
(
B
)B
)B
)B
*B
)*B
*B
)*B
)DB
+B
+6B
,=B
,"B
,"B
,"B
+6B
)_B
,"B
,=B
,=B
-)B
-CB
-CB
./B
.IB
-]B
.IB
/5B
0;B
1AB
1AB
1AB
1AB
1AB
0UB
/OB
1AB
1AB
1vB
1[B
1[B
2|B
2aB
1�B
2|B
1�B
5ZB
6`B
6`B
6`B
5ZB
4nB
4nB
6`B
6`B
6zB
6zB
8lB
7fB
7�B
7�B
8�B
8�B
7�B
8�B
9rB
9�B
:xB
;B
;B
;B
;G�O�B
9�B
<�B
;�B
=�B
>wB
=�B
=�B
<�B
;�B
=�B
>wB
>�B
?}B
?}B
?�B
?�B
?�B
>�B
=�B
=�B
?�B
?�B
@�B
@�B
@�B
?�B
>�B
?�B
?�B
?�B
?�B
>�B
?�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
C�B
E�B
F�B
E�B
G�B
G�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
I�B
J�B
J	B
KB
J�B
K�B
K�B
K�B
L�B
M�B
NB
M�B
M�B
NB
N�B
N�B
N�B
O�B
O�B
O(B
PB
QB
R B
R B
R B
RB
RB
Q4B
RB
S&B
SB
TB
TB
TB
UB
T,B
U2B
VB
W?B
V9B
W$B
W?B
W?B
Y1B
YB
X+B
X+B
X+B
XEB
Y1B
Y1B
YKB
Y1B
Z7B
Z7B
Y1B
YKB
Y1B
Y1B
YKB
Z7B
Z7B
[=B
\CB
\)B
\CB
[=B
\CB
\CB
\CB
[=B
ZQB
[WB
\]B
^OB
_;B
^OB
^OB
_;B
^OB
^OB
_;B
_VB
_VB
`BB
_VB
_VB
_VB
^OB
^jB
_VB
_pB
aHB
bNB
bNB
bhB
aHB
abB
abB
abB
abB
bhB
bNB
bhB
cTB
cTB
cTB
bhB
abB
bhB
b�B
b�B
a|B
cnB
dtB
dtB
dtB
dtB
ezB
ezB
ezB
f�B
f�B
f�B
ffB
ffB
ezB
f�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
hsB
h�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
iyB
i�B
jB
jB
i�B
i�B
i�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
m�B
m�B
o�B
p�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
xB
w�B
x�B
x�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806020038172018060200381720180602003817201806221331132018062213311320180622133113201806042133452018060421334520180604213345  JA  ARFMdecpA19c                                                                20180529063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180528213508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180528213511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180528213512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180528213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180528213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180528213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180528213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180528213514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180528213514                      G�O�G�O�G�O�                JA  ARUP                                                                        20180528215606                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180529153705  CV  JULD            G�O�G�O�F�1�                JM  ARSQJMQC2.0                                                                 20180530000000  CF  PSAL_ADJUSTED_QCC
  D�� G�O�                JM  ARSQJMQC2.0                                                                 20180530000000  CF  TEMP_ADJUSTED_QCC
  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180601153817  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180601153817  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123345  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043113  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                