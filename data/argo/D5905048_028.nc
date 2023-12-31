CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-16T00:35:28Z creation;2016-08-16T00:35:30Z conversion to V3.1;2019-12-19T08:29:12Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160816003528  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_028                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��t'q�1   @��t�`�@4(*�0��d�c�	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@8Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�(�B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B�(�B�(�B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�<)D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�A�A���A���A���A�A�A���A�jA�wA���A�A�ĜA�ƨA�ƨA�A���A�A�wA�jA�9A�bNA��/A�G�A�A��A�ȴAߡ�A�bNA�VA��A�ZAݲ-A���AۑhA�9XA�dZA�ĜA՛�AԓuAҗ�A�~�A�AХ�AуA�7LA�?}A��#A�;dA�^5AˍPAɸRA�G�A���Aĕ�AÁA�ZA�-A�A�K�A� �A��TA��/A��;A�C�A���A��HA�S�A�  A�7LA��jA�1A�p�A�p�A��A��uA�ƨA�JA�Q�A�l�A��9A�G�A�I�A�I�A�S�A�&�A�bNA�G�A�E�A�oA���A�;dA�ƨA���A��A��uA��A�ƨA�A�t�A�E�A�x�A��A�dZA���A���A�"�A���A���A��^A��A��PA~��A|n�Az�Az  Ay�AxJAv�Aq33An�+Am&�Al�AkO�Aj�9Aj�Ai�AiG�Ah��Af�yAe�Aa|�A_�TA^�HA^�+A\ȴAZ�AXĜAX$�AWt�AU�;AS;dAQ
=APAO/AM?}AKƨAJ1'AH�RAG+AE�#AD �AB�jAAhsA@  A>bNA<�+A;dZA:��A9�A8�RA7t�A5�-A3dZA1S�A0��A/�A.A+�
A*bNA)VA(E�A'�PA&��A&5?A%|�A$�`A#�PA"E�A!|�A!&�A E�A��A �A�A�AZA�A��Al�AA�A%A�AA�A\)A��A�Ax�A�9A��A�!Av�A��A�A%A1'Av�AK�A~�AI�AA�PA	�FA��A�
A|�A�A��A�Ar�A�AbA�/A�^A"�A �!@�@��7@��D@��@�@��@�-@�bN@�P@��@�ff@�9@@�S�@�\@���@�dZ@��@�Ĝ@�@��T@�(�@��@�j@ާ�@���@�9X@ۥ�@��@�dZ@��T@�A�@�;d@�33@�+@�$�@�G�@�1@���@�G�@�V@�X@̼j@��@�S�@�M�@�J@���@ɑh@��/@�bN@�A�@��
@�+@�S�@�l�@�o@��y@�v�@��#@ř�@�`B@Ĭ@�\)@���@���@��;@���@��R@�n�@�-@�=q@��#@���@�A�@��;@�K�@��y@�o@��!@��@��#@��@���@���@�@��@���@�t�@��@�V@��@��@�hs@�X@�7L@�/@�z�@���@���@��!@�E�@��^@�^5@���@�`B@���@���@�`B@�Ĝ@���@��`@�t�@�K�@��y@�n�@���@��@�@�K�@��@��P@�@��H@�ȴ@���@��@�7L@��9@�I�@�  @��;@�"�@�J@�J@���@��!@��@���@��@�dZ@�33@���@��@��@�~�@��@��@��@��@��@��@���@��@��@���@�V@��`@���@�;d@�dZ@�t�@��@��F@��w@��@�o@�K�@�@���@���@���@��@�S�@���@��@��y@���@�$�@�{@�@��h@�x�@��7@���@��@�%@�9X@��m@�ƨ@�l�@�K�@�+@��!@��h@��@�V@�V@���@���@�%@�V@��`@�Q�@�9X@� �@�1'@� �@�b@��m@�33@��!@�v�@�ff@�^5@�M�@�-@���@���@��h@�X@���@��j@���@��@�(�@�ƨ@���@�|�@�dZ@�\)@�\)@�S�@��@�ȴ@���@�M�@�5?@�$�@�{@��@�x�@�7L@���@��`@�Ĝ@���@�bN@�9X@�(�@�b@��F@�dZ@�S�@�S�@�;d@�"�@��@��@�~�@��T@��^@���@�/@��u@�  @�t�@�\)@�+@�o@��@��H@�^5@�E�@�V@�V@��7@��@���@�33@�~�@�@��T@�%@��9@��D@�bN@�j@�bN@��@��@�9X@�1@���@�|�@�33@��y@���@��R@��+@�5?@���@�x�@�X@�V@��@�1'@��
@��P@�S�@�33@�"�@��H@��+@�V@�M�@�E�@�$�@�{@��^@�G�@�G�@�G�@�&�@��/@��9@��@�bN@�Z@�I�@��@�@l�@\)@�@~ȴ@~v�@~5?@~@}��@|�@{�m@{@z�!@z~�@z-@z�@zJ@y��@yG�@xr�@w�;@w+@v��@vff@vE�@vE�@v5?@v@u�@u��@u�@uO�@t�/@t�@sƨ@s"�@r��@r�\@rn�@rJ@q�^@q�@p�u@pb@o�@n��@nȴ@nȴ@nv�@n5?@m�-@l�@l�@l��@l��@l�D@lz�@l�@k�
@k��@kt�@kdZ@kS�@k33@ko@j��@j�\@i�^@i�7@i%@h�@hQ�@g�@g��@g��@g;d@g
=@fv�@e�-@eO�@d�@dI�@d(�@cƨ@ct�@cC�@c"�@b=q@`��@`bN@` �@_��@_�P@_+@^�y@^�@^$�@]`B@]/@]/@]/@]V@\��@\9X@[�
@[�F@[dZ@Z��@Z^5@Y�^@Yx�@YX@Y�@XQ�@W�@W��@W�P@V�y@V�R@V��@VV@U�T@U/@T�/@Tj@S�
@S��@S@R�H@R�H@R^5@Q�^@Q�@P��@P��@PA�@Ol�@O+@O
=@N��@M�T@M��@M`B@L�@L1@K33@Ko@J�@I�#@I��@I�@HĜ@Hr�@H �@H  @G|�@G�@F�y@F�@F�R@FV@F$�@F{@F{@F@F@E��@E@E@E��@E�h@E�@D�/@D�D@DI�@D(�@D�@C��@C��@C�@CdZ@C@B��@Bn�@B�@A��@A%@@��@@r�@@Q�@@ �@?�w@?�w@?�w@?�w@?�w@?�w@?��@?�@?\)@>��@>�y@>�R@>��@>ff@>E�@>5?@=�@=p�@=?}@=�@<��@<�/@<��@<Z@<1@;�m@;��@;C�@:��@:=q@:�@9��@9�@9��@9�7@97L@8Ĝ@8�@81'@8 �@7�@7��@7;d@6�y@6�+@6ff@6V@5�T@5`B@5O�@5�@4�/@4�D@4(�@3ƨ@3�@2�@2�!@2=q@2J@1�^@1X@0��@0r�@0Q�@/l�@/
=@.�y@.ȴ@.�R@.��@.�+@.V@.E�@.@-��@-��@-�@,�@,��@,�D@,I�@+��@*�!@*M�@)��@(��@(Ĝ@(bN@( �@'\)@&ȴ@&v�@&V@&$�@&@%@%O�@$�@$�j@$�@$z�@$�@#�F@#t�@#33@#@#@"�H@"^5@!�#@!�^@!��@!hs@ ��@ �u@ A�@ b@   @�@�@+@�@�+@��@O�@?}@/@V@�@��@�j@��@z�@I�@��@�F@��@t�@"�@o@o@o@@�H@�H@�H@��@��@n�@�@�#@�^@X@��@�u@�@r�@1'@b@�@�;@��@|�@l�@;d@�@�R@�+@ff@5?@{@@@�-@�h@?}@�@��@�@��@�D@(�@1@�
@�F@dZ@o@@�@�H@�!@�\@^5@M�@�@J@�#@��@x�@�@%@��@��@��@r�@A�@ �@b@�w@|�@\)@;d@
=@��@�@�R@ff@5?@@@�-@�h@?}@/@�@��@�j@�D@Z@9X@(�@1@�m@�
@�
@ƨ@ƨ@�@S�@33@o@
��@
��@
~�@
~�@
n�@
^5@
^5@
M�@
=q@
-@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�A�A���A���A���A�A�A���A�jA�wA���A�A�ĜA�ƨA�ƨA�A���A�A�wA�jA�9A�bNA��/A�G�A�A��A�ȴAߡ�A�bNA�VA��A�ZAݲ-A���AۑhA�9XA�dZA�ĜA՛�AԓuAҗ�A�~�A�AХ�AуA�7LA�?}A��#A�;dA�^5AˍPAɸRA�G�A���Aĕ�AÁA�ZA�-A�A�K�A� �A��TA��/A��;A�C�A���A��HA�S�A�  A�7LA��jA�1A�p�A�p�A��A��uA�ƨA�JA�Q�A�l�A��9A�G�A�I�A�I�A�S�A�&�A�bNA�G�A�E�A�oA���A�;dA�ƨA���A��A��uA��A�ƨA�A�t�A�E�A�x�A��A�dZA���A���A�"�A���A���A��^A��A��PA~��A|n�Az�Az  Ay�AxJAv�Aq33An�+Am&�Al�AkO�Aj�9Aj�Ai�AiG�Ah��Af�yAe�Aa|�A_�TA^�HA^�+A\ȴAZ�AXĜAX$�AWt�AU�;AS;dAQ
=APAO/AM?}AKƨAJ1'AH�RAG+AE�#AD �AB�jAAhsA@  A>bNA<�+A;dZA:��A9�A8�RA7t�A5�-A3dZA1S�A0��A/�A.A+�
A*bNA)VA(E�A'�PA&��A&5?A%|�A$�`A#�PA"E�A!|�A!&�A E�A��A �A�A�AZA�A��Al�AA�A%A�AA�A\)A��A�Ax�A�9A��A�!Av�A��A�A%A1'Av�AK�A~�AI�AA�PA	�FA��A�
A|�A�A��A�Ar�A�AbA�/A�^A"�A �!@�@��7@��D@��@�@��@�-@�bN@�P@��@�ff@�9@@�S�@�\@���@�dZ@��@�Ĝ@�@��T@�(�@��@�j@ާ�@���@�9X@ۥ�@��@�dZ@��T@�A�@�;d@�33@�+@�$�@�G�@�1@���@�G�@�V@�X@̼j@��@�S�@�M�@�J@���@ɑh@��/@�bN@�A�@��
@�+@�S�@�l�@�o@��y@�v�@��#@ř�@�`B@Ĭ@�\)@���@���@��;@���@��R@�n�@�-@�=q@��#@���@�A�@��;@�K�@��y@�o@��!@��@��#@��@���@���@�@��@���@�t�@��@�V@��@��@�hs@�X@�7L@�/@�z�@���@���@��!@�E�@��^@�^5@���@�`B@���@���@�`B@�Ĝ@���@��`@�t�@�K�@��y@�n�@���@��@�@�K�@��@��P@�@��H@�ȴ@���@��@�7L@��9@�I�@�  @��;@�"�@�J@�J@���@��!@��@���@��@�dZ@�33@���@��@��@�~�@��@��@��@��@��@��@���@��@��@���@�V@��`@���@�;d@�dZ@�t�@��@��F@��w@��@�o@�K�@�@���@���@���@��@�S�@���@��@��y@���@�$�@�{@�@��h@�x�@��7@���@��@�%@�9X@��m@�ƨ@�l�@�K�@�+@��!@��h@��@�V@�V@���@���@�%@�V@��`@�Q�@�9X@� �@�1'@� �@�b@��m@�33@��!@�v�@�ff@�^5@�M�@�-@���@���@��h@�X@���@��j@���@��@�(�@�ƨ@���@�|�@�dZ@�\)@�\)@�S�@��@�ȴ@���@�M�@�5?@�$�@�{@��@�x�@�7L@���@��`@�Ĝ@���@�bN@�9X@�(�@�b@��F@�dZ@�S�@�S�@�;d@�"�@��@��@�~�@��T@��^@���@�/@��u@�  @�t�@�\)@�+@�o@��@��H@�^5@�E�@�V@�V@��7@��@���@�33@�~�@�@��T@�%@��9@��D@�bN@�j@�bN@��@��@�9X@�1@���@�|�@�33@��y@���@��R@��+@�5?@���@�x�@�X@�V@��@�1'@��
@��P@�S�@�33@�"�@��H@��+@�V@�M�@�E�@�$�@�{@��^@�G�@�G�@�G�@�&�@��/@��9@��@�bN@�Z@�I�@��@�@l�@\)@�@~ȴ@~v�@~5?@~@}��@|�@{�m@{@z�!@z~�@z-@z�@zJ@y��@yG�@xr�@w�;@w+@v��@vff@vE�@vE�@v5?@v@u�@u��@u�@uO�@t�/@t�@sƨ@s"�@r��@r�\@rn�@rJ@q�^@q�@p�u@pb@o�@n��@nȴ@nȴ@nv�@n5?@m�-@l�@l�@l��@l��@l�D@lz�@l�@k�
@k��@kt�@kdZ@kS�@k33@ko@j��@j�\@i�^@i�7@i%@h�@hQ�@g�@g��@g��@g;d@g
=@fv�@e�-@eO�@d�@dI�@d(�@cƨ@ct�@cC�@c"�@b=q@`��@`bN@` �@_��@_�P@_+@^�y@^�@^$�@]`B@]/@]/@]/@]V@\��@\9X@[�
@[�F@[dZ@Z��@Z^5@Y�^@Yx�@YX@Y�@XQ�@W�@W��@W�P@V�y@V�R@V��@VV@U�T@U/@T�/@Tj@S�
@S��@S@R�H@R�H@R^5@Q�^@Q�@P��@P��@PA�@Ol�@O+@O
=@N��@M�T@M��@M`B@L�@L1@K33@Ko@J�@I�#@I��@I�@HĜ@Hr�@H �@H  @G|�@G�@F�y@F�@F�R@FV@F$�@F{@F{@F@F@E��@E@E@E��@E�h@E�@D�/@D�D@DI�@D(�@D�@C��@C��@C�@CdZ@C@B��@Bn�@B�@A��@A%@@��@@r�@@Q�@@ �@?�w@?�w@?�w@?�w@?�w@?�w@?��@?�@?\)@>��@>�y@>�R@>��@>ff@>E�@>5?@=�@=p�@=?}@=�@<��@<�/@<��@<Z@<1@;�m@;��@;C�@:��@:=q@:�@9��@9�@9��@9�7@97L@8Ĝ@8�@81'@8 �@7�@7��@7;d@6�y@6�+@6ff@6V@5�T@5`B@5O�@5�@4�/@4�D@4(�@3ƨ@3�@2�@2�!@2=q@2J@1�^@1X@0��@0r�@0Q�@/l�@/
=@.�y@.ȴ@.�R@.��@.�+@.V@.E�@.@-��@-��@-�@,�@,��@,�D@,I�@+��@*�!@*M�@)��@(��@(Ĝ@(bN@( �@'\)@&ȴ@&v�@&V@&$�@&@%@%O�@$�@$�j@$�@$z�@$�@#�F@#t�@#33@#@#@"�H@"^5@!�#@!�^@!��@!hs@ ��@ �u@ A�@ b@   @�@�@+@�@�+@��@O�@?}@/@V@�@��@�j@��@z�@I�@��@�F@��@t�@"�@o@o@o@@�H@�H@�H@��@��@n�@�@�#@�^@X@��@�u@�@r�@1'@b@�@�;@��@|�@l�@;d@�@�R@�+@ff@5?@{@@@�-@�h@?}@�@��@�@��@�D@(�@1@�
@�F@dZ@o@@�@�H@�!@�\@^5@M�@�@J@�#@��@x�@�@%@��@��@��@r�@A�@ �@b@�w@|�@\)@;d@
=@��@�@�R@ff@5?@@@�-@�h@?}@/@�@��@�j@�D@Z@9X@(�@1@�m@�
@�
@ƨ@ƨ@�@S�@33@o@
��@
��@
~�@
~�@
n�@
^5@
^5@
M�@
=q@
-@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�fB
��B
=B
=BJBJBPBVB	7B	7BB
��B
��B
�B
�yB
�;B
��B
�/B
�)B
�B
�!B
��BhBYBt�BZBT�B]/Bq�By�B�5BA�Bx�B�?B��B��B�HB�B��BB+B%B\BPB+BB�B�B��BȴBɺB��BɺB�}B�RB��B~�B`BBYB_;BP�BXBVBI�B8RBoB��B�FB�9B�B��B�-B��B�B��B�bB{�Bl�BR�B2-B�BB
�B
�`B
�NB
�HB
��B
��B
�-B
��B
�VB
o�B
aHB
T�B
K�B
D�B
=qB
0!B
{B	��B	�B	�sB	�HB	�/B	�B	��B	��B	��B	B	�RB	��B	��B	�oB	�\B	�+B	y�B	p�B	jB	gmB	]/B	T�B	F�B	A�B	>wB	7LB	/B	%�B	�B	�B	�B	oB	JB	B��B�B�B�sB�mB�TB�5B�B��B��BɺBǮBŢBÖB�qB�dB�jB�qB�qB�qB�jB�dB�^B�XB�9B�'B�!B�B�B�jB�wB��BBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴB��B��BŢBB�wB�^B�LB�LB�dB��B��B�wB�qB�^B�^B�LB�9B�-B�3B�-B�!B�B�B�B�!B�B�B�B�!B�B�B�B�B�B�B�-B�RB�qB�qB�wB�qB�^B�RB�LB�?B�9B�?B�?B�9B�?B�FB�XB�dB�qBB��B��B��B��B��B��B�
B��B�B�)B�NB�ZB�fB�`B�ZB�`B�fB�B�B��B��B��B��B	B	DB	hB	hB	oB	oB	�B	�B	�B	�B	�B	{B	{B	�B	�B	�B	�B	#�B	(�B	.B	0!B	/B	/B	33B	49B	49B	49B	6FB	:^B	:^B	:^B	<jB	=qB	=qB	<jB	<jB	<jB	B�B	B�B	I�B	M�B	O�B	P�B	W
B	gmB	gmB	cTB	dZB	e`B	jB	v�B	�B	�JB	�PB	�DB	�oB	�+B	�B	�B	�B	�%B	�B	�PB	�uB	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�-B	�9B	�?B	�LB	�RB	�^B	�XB	�^B	�}B	��B	��B	�qB	�^B	�RB	�LB	�FB	�dB	�qB	�qB	�jB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�HB	�TB	�ZB	�`B	�`B	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
	7B
	7B
	7B
	7B
DB
PB
VB
VB
VB
VB
\B
bB
\B
PB
JB
	7B
+B
%B
%B
B
B
%B
+B
1B
	7B

=B
JB
PB
PB
VB
VB
VB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
+B
+B
,B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
/B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
XB
W
B
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
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
jB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�OB
�OB
ބB
�B
�B
��B
�B
rB�B�B�B�B	�B
rB�B
��B
��B
�UB
�B
�B
��B
�'B
�B
�B
��B
��B�BZBxB[qBV�B_�ButB}�B�HBD�B|�B�B��BҽB�ZB�B��B�B�B1B�B�B
	B�B��B�vB��B�rB˒B�}B�PB�{B��B��B�uBb�B[WBbBS[BZkBXyBMB<�B+B�SB��B��B�UB��B��B�B��B�B�&B~�Bp�BW�B6FB�BfB
�B
�2B
�&B
�ZB
�B
�3B
��B
��B
��B
r-B
cB
VSB
M6B
F�B
@�B
5?B
YB	��B	��B	�_B	�B	��B	��B	ԕB	�TB	�B	�SB	�B	��B	��B	��B	��B	�	B	{dB	q�B	k�B	i�B	`\B	WYB	H1B	CB	@�B	9>B	1'B	'�B	 �B	xB	�B	FB	"B	B�B��B��B�B�B�B�'BܬB��B�&B��B�7B�KB�?B�cB�B��B�wB��B�(B�qB��B�B��B�?B��B�'B�wB�cB�"B�}B�'B�B�9B�~B��B�"B�B˒B�~B��BΥB��B��B�B�BB��B�xB�=B��B�B�+BĶB��B�JB��B�B��B��B��B��B�B�dB�B�lB��B��B�9B��B��B� B�B��B�'B��B�B��B��B�B�/B��B��B��B�;B��B��B�(B�]B��B�wB�dB��B�RB��B�tB��B�zB�tB��B��B��B�B��BÖB�VB�B�.BѷB��B�B�+BՁB�9B�]B��B��B�B��B�B�B��B�B��B��B�8B�dB��B	-B	�B	�B	�B	�B	�B	�B	9B	�B	�B	SB	�B	�B	B	�B	�B	�B	$@B	)�B	.�B	0oB	/�B	/�B	33B	4�B	4�B	4TB	6`B	:xB	:�B	:�B	="B	=�B	=�B	<�B	<�B	<�B	B�B	B�B	I�B	M�B	O�B	QNB	V�B	hXB	hsB	c�B	dtB	d�B	i�B	u�B	��B	��B	��B	�JB	�B	��B	��B	�UB	�SB	�YB	�B	�PB	�[B	�yB	��B	��B	�DB	�"B	�/B	�IB	��B	��B	��B	��B	��B	��B	��B	��B	�XB	�*B	��B	�'B	�AB	�B	��B	��B	��B	�FB	��B	��B	��B	��B	�	B	�(B	�VB	�B	�(B	�4B	��B	��B	�B	�:B	҉B	�NB	��B	�B	��B	��B	�$B	�eB	�EB	�B	�QB	�#B	��B	�HB	�nB	�B	�B	�zB	�zB	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�$B	��B	�B	��B	��B	��B	�B	�0B	�0B	�B	�PB	�"B	�B	�BB	�BB	�HB
 B
 B
 B
B
 B
;B
 4B
UB
;B
AB
'B
'B
'B
[B
aB
GB
MB
3B
3B
3B
MB
9B
9B
9B
�B
SB
?B
%B
YB
YB
?B
YB
�B
�B
YB
_B
�B
�B
	�B
	�B
	RB
	lB
^B
jB
�B
�B
pB
VB
�B
�B
�B
�B
PB
	�B
zB
YB
�B
mB
9B
?B
+B
1B
	7B

rB
~B
�B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 'B
 B
 �B
 �B
 �B
!�B
!�B
!�B
"B
# B
#B
# B
$B
$B
$B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
%B
%,B
&B
&B
&B
'B
'B
'B
(>B
($B
)*B
)*B
)*B
*0B
*B
+B
+B
+B
+6B
+6B
,"B
,B
+B
+B
,"B
+6B
,"B
,"B
,"B
,B
,"B
,"B
,"B
,"B
,=B
-]B
./B
.IB
.IB
/5B
/5B
/OB
/5B
/OB
0;B
/iB
0oB
1[B
1vB
2GB
2GB
2aB
2GB
2GB
2aB
2�B
3�B
4nB
5ZB
5ZB
5ZB
5tB
6`B
6`B
6�B
7�B
8lB
8RB
8RB
8lB
8�B
8�B
8lB
8�B
8�B
8�B
9�B
9�B
9rB
9rB
9�B
9�B
9rB
9rB
9rB
9�B
:xB
:�B
:�B
:�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
MB
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O(B
PB
O�B
Q B
Q B
Q B
Q B
QB
Q B
QB
RB
R B
RB
R B
RB
R B
R B
SB
SB
SB
S&B
T,B
TB
TB
TB
TB
TFB
U2B
UB
U2B
VB
V9B
VB
V9B
W?B
WYB
X+B
W?B
X_B
Y1B
Y1B
Y1B
YB
YB
Y1B
Y1B
Y1B
YKB
Z7B
Z7B
ZQB
ZQB
[#B
[=B
[WB
[qB
\xB
\]B
\]B
]dB
^OB
^jB
^jB
^�B
_pB
_VB
`\B
`vB
`vB
`\B
`�B
abB
abB
abB
abB
a|B
a|B
b�B
bhB
bhB
bNB
bhB
b�B
c�B
c�B
cnB
cnB
c�B
dtB
dtB
dZB
dZB
dtB
dtB
e�B
ezB
e�B
f�B
f�B
gmB
gmB
g�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
hsB
hsB
hsB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608200036122016082000361220160820003612201806221300502018062213005020180622130050201804050659492018040506594920180405065949  JA  ARFMdecpA19c                                                                20160816093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160816003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160816003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160816003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160816003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160816003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160816003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160816003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160816003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160816003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20160816011859                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160816153720  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160816153720  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160816153720  CV  LATITUDE        G�O�G�O�A�X                JM  ARGQJMQC2.0                                                                 20160816153720  CV  LONGITUDE       G�O�G�O��$�u                JM  ARCAJMQC2.0                                                                 20160819153612  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160819153612  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215949  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040050  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                