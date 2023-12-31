CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-13T21:35:18Z creation;2016-12-13T21:35:24Z conversion to V3.1;2019-12-19T08:19:47Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161213213518  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA  I2_0577_068                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��r�� 1   @��shK� @3��$��d��/��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C�HC��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_�RD`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A՟�Aգ�Aգ�Aա�A՟�Aգ�Aհ!Aմ9AնFAռjAպ^AՍPA��`Aԡ�Aԗ�Aԉ7Aԣ�AԴ9AԶFA���A��
A��/A��
AԬA��AӴ9A�I�A��A҅A��A���A�Aѧ�A�r�A�^5A�^5A�A�A�A��`A��;A���AС�A�5?Aϴ9A;wA��`AȶFAƾwA�oA�bNA���A��-A��A��PA��\A�ffA�+A�5?A��A��7A���A���A���A���A�^5A���A�;dA��A�n�A��A��TA�l�A�+A�  A���A�C�A��FA���A��A�x�A��^A�A���A��TA��
A�|�A��wA��^A�VA��`A�A�S�A���A��`A�JA��A�1A�M�A��9A�XA�ĜA�%A�A���A��\A�l�A�K�A��/A��FA�n�A���A���A�"�A���A�C�AA{�Ay�Av�!At�As��Ar��Arz�Ar  Aq�wAqG�Ao�AmS�AjjAh  Af�jAeAc�;Ac�^Ab-Aa+A^��A]�A\jA[�^A[�AYhsAX1AW?}AVffAUS�AS�AR  AQ+AP�AP �ANZALr�AKdZAJ~�AH$�AFr�AD�ADz�AB{AA&�A?�A?%A>bA;��A:�A:�jA:(�A9K�A8ĜA7�A7�A5"�A3�mA3O�A2=qA/x�A.{A-A+��A+G�A++A*��A*M�A)��A(n�A'/A&E�A%x�A%+A$��A$��A#oA��A��AVAO�AƨA�jA�A�HAffAA�A�TA7LA�A+A��A�jAn�A
��A
5?A	��A�+A�HA�hA��A�RA��A�AC�A ��A z�A $�@��;@�C�@���@�@�  @��@���@�M�@���@�V@�u@���@�@�x�@���@���@�@�`B@�r�@��@�E�@��@�?}@�dZ@�-@��@�7L@�b@�33@��#@ݑh@�&�@�I�@۶F@�@أ�@��@և+@�ff@�=q@�5?@�p�@ԃ@��@�ȴ@�O�@� �@�
=@���@Ώ\@�-@�p�@�%@�(�@��
@�ƨ@˕�@�S�@ʗ�@�E�@��@�O�@��;@�|�@�;d@��@��T@��@ċD@� �@���@¸R@�$�@���@�p�@�O�@���@�Z@��w@��@�33@�{@���@��@��@��w@��@�ƨ@�C�@��@�Q�@��@�33@��@�E�@��h@��7@�p�@�G�@��@��@��D@�9X@�ƨ@���@�E�@�$�@�J@�&�@��@�b@���@�K�@���@���@��R@��@���@�`B@�`B@���@�n�@���@���@�5?@�G�@��@�Z@�Z@�I�@�b@���@�+@�ff@�J@�-@�ff@�33@��F@�t�@���@�n�@�n�@��+@��!@�
=@�K�@��@���@�v�@��@��@���@�/@�O�@��/@�j@���@��@�1@�K�@� �@�  @��@�S�@��H@���@��+@��@��T@��h@�x�@���@� �@�1@��m@�t�@�%@��u@���@�9X@��@�t�@�S�@��!@�5?@��T@���@�X@�G�@�Ĝ@��D@�Z@�I�@�  @��w@��@�\)@�
=@��H@��!@�=q@�@�&�@���@��@���@��@�Q�@�(�@�ƨ@�dZ@���@��\@�n�@�M�@�E�@�@��^@���@�x�@��@�bN@�S�@�
=@���@�-@��@�n�@�5?@���@��@���@��^@�`B@���@�bN@��m@���@��@�t�@�K�@�@�n�@��#@�`B@��@���@�t�@�;d@�33@���@�-@�J@���@��^@��7@��@�p�@�O�@�/@���@��j@��j@��j@��@���@��D@�A�@��;@��F@�|�@�"�@��@��H@��R@�ff@�-@��#@�x�@���@���@���@�z�@�1@��@��m@���@���@�t�@�|�@�l�@�33@���@��R@�v�@�E�@�J@��^@���@�p�@�V@���@�9X@�(�@�@�@;d@�@~ȴ@~�+@~E�@~{@}�-@}��@}p�@}V@|�j@|��@|��@|j@|I�@|1@{��@{33@{o@{@z�@y��@y%@x�9@xb@w+@v��@v$�@up�@u�@t�@t��@s�m@r�@r~�@r�@q�#@q�^@q�^@qx�@qX@qG�@q%@pr�@pb@o�w@o��@o|�@oK�@o
=@nv�@m�@l�@l��@lj@lI�@kƨ@kS�@j��@j~�@j-@i��@ihs@h�`@g�@g\)@f��@fE�@f$�@f$�@f@e�-@ep�@d��@d�@c��@cC�@c@b�H@b�\@a��@a7L@_�@_|�@_+@^��@^ȴ@^��@^v�@^@]�h@]/@\�j@\j@\1@[�F@[�@[t�@[dZ@[C�@Z��@Z��@Z~�@Z�@Y�^@Y�7@YX@Y%@W��@W
=@Vȴ@V@U��@U�@T�j@TZ@T(�@S�m@S�F@St�@SdZ@SS�@S@Rn�@RM�@Q��@Q��@Q�^@Qx�@QG�@Q7L@P��@Pr�@PA�@P  @O��@O\)@O
=@N�R@N��@NE�@M�T@M�@M�@M`B@L�j@Lz�@LI�@K��@K�F@Kt�@J�H@J��@J��@J~�@I�#@IX@H�`@HQ�@G�P@F��@E�T@E�h@D��@DZ@D9X@D�@C�m@Cƨ@Co@B��@B�H@B��@B�!@BM�@A��@Ax�@Ahs@A&�@@��@@��@@bN@@ �@?�;@?|�@>��@>V@>E�@>5?@>@=@=O�@<��@<�@;�
@;��@;��@;t�@;"�@:��@9��@97L@8�`@8��@8��@8�@8bN@8Q�@81'@8  @7��@7
=@6ȴ@6�@6ȴ@6��@6�+@6v�@6v�@65?@5O�@5�@4�/@4��@4j@4�@3�
@3�
@3�F@3t�@3o@3@2�H@2��@2n�@2M�@2-@2�@2J@1��@1��@1x�@1X@1&�@0��@0��@0bN@0bN@0bN@0A�@0  @/�P@/|�@/|�@/|�@/;d@.��@.��@.@-�T@-��@-/@,�j@,�j@,Z@,�@,1@+�m@+ƨ@+��@+S�@+33@+"�@+o@*�H@*�!@*n�@)��@)��@)��@)�7@)x�@)x�@)hs@)G�@)%@(Ĝ@(A�@( �@'�w@'�P@';d@&�y@&5?@%�@$��@#��@#S�@"�H@"n�@"-@!�#@!�#@!�#@!�^@!��@!x�@!G�@!%@ �`@ Ĝ@ �u@ r�@ A�@�@�w@�@�w@�@\)@�y@v�@��@p�@?}@V@V@��@�@��@��@�@1@ƨ@��@�@t�@dZ@dZ@dZ@S�@"�@�@�!@^5@�@�@J@�@��@��@�@%@�`@Ĝ@��@�u@�@Q�@b@b@��@�@��@|�@l�@l�@+@��@�+@5?@$�@@��@�T@�T@�-@?}@/@�@V@V@��@�@�j@�D@(�@�@1@��@��@�
@�
@ƨ@ƨ@ƨ@ƨ@ƨ@dZ@S�@33@33@o@��@n�@-@-@-@-@-@�@J@�^@x�@G�@7L@&�@�@r�@1'@1'@ �@b@�;@�P@\)@K�@K�@K�@K�@;d@;d@
=@��@�@v�@ff@ff@E�@5?@{@��@`B@�@V@�/@�j@��@Z@�@�
@�@33@
�@
�\@
J@	��@	�^@	�7@	7L@��@��@��@��@��@�u@r�@A�@A�@ �@b@  @�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A՟�Aգ�Aգ�Aա�A՟�Aգ�Aհ!Aմ9AնFAռjAպ^AՍPA��`Aԡ�Aԗ�Aԉ7Aԣ�AԴ9AԶFA���A��
A��/A��
AԬA��AӴ9A�I�A��A҅A��A���A�Aѧ�A�r�A�^5A�^5A�A�A�A��`A��;A���AС�A�5?Aϴ9A;wA��`AȶFAƾwA�oA�bNA���A��-A��A��PA��\A�ffA�+A�5?A��A��7A���A���A���A���A�^5A���A�;dA��A�n�A��A��TA�l�A�+A�  A���A�C�A��FA���A��A�x�A��^A�A���A��TA��
A�|�A��wA��^A�VA��`A�A�S�A���A��`A�JA��A�1A�M�A��9A�XA�ĜA�%A�A���A��\A�l�A�K�A��/A��FA�n�A���A���A�"�A���A�C�AA{�Ay�Av�!At�As��Ar��Arz�Ar  Aq�wAqG�Ao�AmS�AjjAh  Af�jAeAc�;Ac�^Ab-Aa+A^��A]�A\jA[�^A[�AYhsAX1AW?}AVffAUS�AS�AR  AQ+AP�AP �ANZALr�AKdZAJ~�AH$�AFr�AD�ADz�AB{AA&�A?�A?%A>bA;��A:�A:�jA:(�A9K�A8ĜA7�A7�A5"�A3�mA3O�A2=qA/x�A.{A-A+��A+G�A++A*��A*M�A)��A(n�A'/A&E�A%x�A%+A$��A$��A#oA��A��AVAO�AƨA�jA�A�HAffAA�A�TA7LA�A+A��A�jAn�A
��A
5?A	��A�+A�HA�hA��A�RA��A�AC�A ��A z�A $�@��;@�C�@���@�@�  @��@���@�M�@���@�V@�u@���@�@�x�@���@���@�@�`B@�r�@��@�E�@��@�?}@�dZ@�-@��@�7L@�b@�33@��#@ݑh@�&�@�I�@۶F@�@أ�@��@և+@�ff@�=q@�5?@�p�@ԃ@��@�ȴ@�O�@� �@�
=@���@Ώ\@�-@�p�@�%@�(�@��
@�ƨ@˕�@�S�@ʗ�@�E�@��@�O�@��;@�|�@�;d@��@��T@��@ċD@� �@���@¸R@�$�@���@�p�@�O�@���@�Z@��w@��@�33@�{@���@��@��@��w@��@�ƨ@�C�@��@�Q�@��@�33@��@�E�@��h@��7@�p�@�G�@��@��@��D@�9X@�ƨ@���@�E�@�$�@�J@�&�@��@�b@���@�K�@���@���@��R@��@���@�`B@�`B@���@�n�@���@���@�5?@�G�@��@�Z@�Z@�I�@�b@���@�+@�ff@�J@�-@�ff@�33@��F@�t�@���@�n�@�n�@��+@��!@�
=@�K�@��@���@�v�@��@��@���@�/@�O�@��/@�j@���@��@�1@�K�@� �@�  @��@�S�@��H@���@��+@��@��T@��h@�x�@���@� �@�1@��m@�t�@�%@��u@���@�9X@��@�t�@�S�@��!@�5?@��T@���@�X@�G�@�Ĝ@��D@�Z@�I�@�  @��w@��@�\)@�
=@��H@��!@�=q@�@�&�@���@��@���@��@�Q�@�(�@�ƨ@�dZ@���@��\@�n�@�M�@�E�@�@��^@���@�x�@��@�bN@�S�@�
=@���@�-@��@�n�@�5?@���@��@���@��^@�`B@���@�bN@��m@���@��@�t�@�K�@�@�n�@��#@�`B@��@���@�t�@�;d@�33@���@�-@�J@���@��^@��7@��@�p�@�O�@�/@���@��j@��j@��j@��@���@��D@�A�@��;@��F@�|�@�"�@��@��H@��R@�ff@�-@��#@�x�@���@���@���@�z�@�1@��@��m@���@���@�t�@�|�@�l�@�33@���@��R@�v�@�E�@�J@��^@���@�p�@�V@���@�9X@�(�@�@�@;d@�@~ȴ@~�+@~E�@~{@}�-@}��@}p�@}V@|�j@|��@|��@|j@|I�@|1@{��@{33@{o@{@z�@y��@y%@x�9@xb@w+@v��@v$�@up�@u�@t�@t��@s�m@r�@r~�@r�@q�#@q�^@q�^@qx�@qX@qG�@q%@pr�@pb@o�w@o��@o|�@oK�@o
=@nv�@m�@l�@l��@lj@lI�@kƨ@kS�@j��@j~�@j-@i��@ihs@h�`@g�@g\)@f��@fE�@f$�@f$�@f@e�-@ep�@d��@d�@c��@cC�@c@b�H@b�\@a��@a7L@_�@_|�@_+@^��@^ȴ@^��@^v�@^@]�h@]/@\�j@\j@\1@[�F@[�@[t�@[dZ@[C�@Z��@Z��@Z~�@Z�@Y�^@Y�7@YX@Y%@W��@W
=@Vȴ@V@U��@U�@T�j@TZ@T(�@S�m@S�F@St�@SdZ@SS�@S@Rn�@RM�@Q��@Q��@Q�^@Qx�@QG�@Q7L@P��@Pr�@PA�@P  @O��@O\)@O
=@N�R@N��@NE�@M�T@M�@M�@M`B@L�j@Lz�@LI�@K��@K�F@Kt�@J�H@J��@J��@J~�@I�#@IX@H�`@HQ�@G�P@F��@E�T@E�h@D��@DZ@D9X@D�@C�m@Cƨ@Co@B��@B�H@B��@B�!@BM�@A��@Ax�@Ahs@A&�@@��@@��@@bN@@ �@?�;@?|�@>��@>V@>E�@>5?@>@=@=O�@<��@<�@;�
@;��@;��@;t�@;"�@:��@9��@97L@8�`@8��@8��@8�@8bN@8Q�@81'@8  @7��@7
=@6ȴ@6�@6ȴ@6��@6�+@6v�@6v�@65?@5O�@5�@4�/@4��@4j@4�@3�
@3�
@3�F@3t�@3o@3@2�H@2��@2n�@2M�@2-@2�@2J@1��@1��@1x�@1X@1&�@0��@0��@0bN@0bN@0bN@0A�@0  @/�P@/|�@/|�@/|�@/;d@.��@.��@.@-�T@-��@-/@,�j@,�j@,Z@,�@,1@+�m@+ƨ@+��@+S�@+33@+"�@+o@*�H@*�!@*n�@)��@)��@)��@)�7@)x�@)x�@)hs@)G�@)%@(Ĝ@(A�@( �@'�w@'�P@';d@&�y@&5?@%�@$��@#��@#S�@"�H@"n�@"-@!�#@!�#@!�#@!�^@!��@!x�@!G�@!%@ �`@ Ĝ@ �u@ r�@ A�@�@�w@�@�w@�@\)@�y@v�@��@p�@?}@V@V@��@�@��@��@�@1@ƨ@��@�@t�@dZ@dZ@dZ@S�@"�@�@�!@^5@�@�@J@�@��@��@�@%@�`@Ĝ@��@�u@�@Q�@b@b@��@�@��@|�@l�@l�@+@��@�+@5?@$�@@��@�T@�T@�-@?}@/@�@V@V@��@�@�j@�D@(�@�@1@��@��@�
@�
@ƨ@ƨ@ƨ@ƨ@ƨ@dZ@S�@33@33@o@��@n�@-@-@-@-@-@�@J@�^@x�@G�@7L@&�@�@r�@1'@1'@ �@b@�;@�P@\)@K�@K�@K�@K�@;d@;d@
=@��@�@v�@ff@ff@E�@5?@{@��@`B@�@V@�/@�j@��@Z@�@�
@�@33@
�@
�\@
J@	��@	�^@	�7@	7L@��@��@��@��@��@�u@r�@A�@A�@ �@b@  @�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
)�B
.B
8RB
_;B
��B
�wB
��B
�)B
�BB�B2-B7LB=qBF�Bm�B��B!�B"�B$�B"�B=qBP�B^5B_;BbNBbNBffBffBbNB_;B`BB_;BbNBl�Bm�B�B�%B�oB��B�JB��B��B�B�-BB��B��B��B�B��B�B�TB��B�qB��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�=B�B}�Be`BD�B49B�B	7B�BǮB��B��B��B�7B�B�hB�}BǮB�wB�RB�B��B��B�PB�BffBQ�BG�BD�B@�B7LB$�B1B
�TB
��B
ƨB
�'B
��B
}�B
`BB
K�B
<jB
-B
$�B
 �B
�B
�B
�B
�B
VB
  B	�B	�TB	�HB	�B	��B	��B	ȴB	�wB	�'B	��B	��B	��B	��B	�\B	�+B	�B	{�B	t�B	l�B	cTB	]/B	[#B	VB	K�B	=qB	5?B	0!B	'�B	 �B	�B	�B	\B	JB	DB	%B	B��B��B��B��B�B�B�B�B�`B�TB�HB�;B�;B�B�B�B�B�B�B�B�B�B�
B�
B��B��B��B��B��BǮBŢB��B�jB�9B�!B��B��B��B��B��B��B��B�oB�\B�PB�=B�1B�JB�VB�\B�1B�1B�DB�7B�1B�+B�1B�=B�=B�DB�JB�PB�VB�\B�hB�hB�bB�VB�JB�DB�PB�JB�JB�JB�DB�VB�VB�bB�oB�hB�hB�hB�oB��B��B��B��B��B��B��B��B�!B�9B�?B�?B�RB�XB�^B�^B�^B�^B�qB�}B��BƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�#B�)B�/B�;B�HB�TB�ZB�`B�sB�B�B�B�B��B��B	B	1B	1B		7B		7B	DB	DB	DB	PB	bB	oB	�B	�B	�B	�B	"�B	#�B	&�B	+B	+B	,B	,B	0!B	49B	6FB	7LB	:^B	=qB	?}B	C�B	E�B	E�B	F�B	I�B	K�B	L�B	N�B	O�B	O�B	S�B	XB	ZB	]/B	`BB	dZB	ffB	gmB	hsB	iyB	iyB	iyB	iyB	iyB	jB	k�B	l�B	m�B	p�B	t�B	w�B	~�B	�B	�DB	�DB	�DB	�DB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�?B	�FB	�RB	�qB	�qB	�}B	�}B	��B	B	B	ĜB	ǮB	ƨB	ƨB	ƨB	ŢB	��B	�}B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�5B	�;B	�HB	�HB	�NB	�TB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
hB
oB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
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
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
H�B
I�B
I�B
H�B
I�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
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
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
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
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
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
ZB
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
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
jB
jB
jB
jB
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
l�B
l�B
l�B
l�B
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
o�B
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
q�B
r�B
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
s�B
t�B
t�B
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
w�B
w�B
w�B
w�B
w�B
w�B
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
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
|�B
|�B
|�B
}�B
|�B
}�B
|�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
*B
.IB
9$B
`�B
��B
�wB
��B
�B
��BB�B1�B7LB=�BG_BoB��B"4B#nB%�B#�B=�BQNB^jB_�BbhBbhBf�Bf�Bb�B_pB`�B_�Bc�Bn�Br�B��B�)B��B��B�.B��B�B��B��B�9BּB�B��B�iB�B�ZB�2B�B��B�eB�kB��B��B��B��B�B��B�CB�#B�B��B�B��B��B�B��Bi*BHB8B#B�B��B�XB��B�`B�EB�=B��B�B�oBʦB��B��B�iB�B�]B��B��Bh�BR�BH1BESBBAB:�B)�B�B
��B
�,B
�XB
��B
��B
��B
cTB
N�B
>�B
.IB
%�B
!bB
OB
=B
�B
�B
NB
aB	�[B	�,B	�:B	�QB	ѷB	ҽB	�rB	�;B	��B	��B	��B	��B	��B	� B	�KB	�AB	}qB	v�B	n�B	dZB	^B	\xB	X_B	M�B	>�B	6�B	2�B	*B	"�B	�B	B	�B	�B	�B	�B	mB��B�FB��B��B��B��B�!B��B�B�B�:B�hB��BڠB�kBخB�_BٴB��B�B��BںB�EB��B՛B��B��BՁB�sBȴB��B�'B�BB��B��B�B��B�&B�~B��B�#B�
B�aB�NB�B�B�lB�jB�B��B�XB��B��B�rB��B��B�B��B��B��B��B��B�BB��B��B��B��B��B��B�~B�VB�B��B��B�JB��B��B�4B�[B��B��B�B��B�kB�5B�NB��B��B��B�LB�yB��B��B�zB�`B�>B��B��B��B��B��B�(B� B��BǮBʌB�~B�B�B�PB�PB�BB�vB� B� B�&B�[B�{B�SB�mB֡B��B�qBܒBݘB��B��B�B��B��B�*B��B��B��B��B�B�xB	�B	�B	�B	
#B	
XB	xB	^B	DB	jB	�B	&B	�B	#B	/B	VB	# B	$ZB	'RB	+B	+B	,=B	,WB	0;B	4�B	6�B	7�B	:�B	=�B	?�B	C�B	F?B	F%B	GB	J	B	K�B	MB	OB	PB	PbB	TFB	XEB	ZB	\�B	_�B	dZB	f�B	g�B	iB	i�B	i�B	i�B	i�B	i�B	j�B	l"B	mB	m�B	p�B	t�B	wfB	~�B	�mB	��B	�^B	�DB	�DB	�VB	�(B	�hB	��B	��B	��B	��B	��B	��B	�B	��B	�,B	�$B	��B	�WB	�}B	�}B	��B	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	��B	��B	��B	�EB	�B	��B	��B	�B	�)B	��B	�B	�\B	�.B	�B	�.B	�B	�B	�.B	�4B	�B	�&B	�KB	�xB	�dB	�jB	�pB	�|B	�|B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	�B	�9B	�GB	�B	��B	�B	�B	��B	�B	�B	�B	�B
 B
 OB
 OB
 iB
uB
-B
-B
GB
aB
aB
{B
�B
�B
�B
�B
 4B	�.B	�B	�wB	�cB
 B
 B
AB
-B
-B
-B
GB
3B
MB
?B
%B
%B
?B
EB
_B
_B
�B

XB

rB
xB
~B
dB
~B
�B
�B
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
 BB
!B
!�B
"B
# B
# B
#:B
#B
"�B
#�B
$B
$&B
$&B
%B
$B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&2B
'B
'B
'B
'B
(
B
($B
($B
($B
(XB
)B
*B
*B
*0B
*KB
*0B
+B
,"B
,=B
,WB
,qB
-]B
.IB
.IB
/5B
/5B
/B
/5B
/5B
/OB
/iB
0UB
0UB
0;B
0UB
1AB
1[B
1vB
1vB
2�B
3hB
3hB
4TB
4TB
4TB
4TB
4nB
4nB
5tB
5�B
6`B
6`B
6`B
6`B
7LB
7fB
7�B
7�B
7fB
7fB
7�B
8lB
8�B
8�B
8�B
8�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
<�B
<�B
<jB
;�B
;�B
;�B
<�B
<�B
<�B
<�B
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
?�B
?}B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
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
H�B
I�B
I�B
H�B
I�B
H�B
I�B
I�B
KB
K�B
K�B
K�B
K�B
K�B
MB
MB
NB
M�B
M�B
M�B
M�B
NB
NB
N"B
M�B
M�B
NB
M�B
N�B
N�B
N�B
OB
N�B
OB
PB
O�B
O�B
O�B
Q B
Q B
Q B
Q B
QB
QNB
RB
RB
RB
RB
RB
RB
R�B
T,B
TB
UB
UB
UB
UB
UB
VB
VB
VB
W
B
W$B
W$B
W$B
W$B
X+B
X+B
X+B
X+B
XB
Y1B
Y1B
YKB
Y1B
ZB
Z7B
ZB
Z7B
Z7B
ZkB
ZQB
[=B
[=B
[WB
[=B
\)B
\CB
\CB
\)B
\CB
\CB
]IB
]dB
^OB
^5B
^OB
^OB
_VB
_VB
_pB
_VB
_;B
_;B
_;B
_;B
_VB
_VB
_VB
_VB
`vB
`\B
`\B
abB
abB
a|B
a�B
b�B
b�B
c�B
c�B
c�B
c�B
dtB
ezB
ezB
e`B
ezB
ezB
ezB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
hsB
hsB
h�B
h�B
h�B
h�B
iyB
i�B
i�B
iyB
iyB
iyB
jB
iyB
i�B
j�B
j�B
j�B
j�B
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
l�B
l�B
l�B
l�B
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
o�B
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
q�B
r�B
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
s�B
t�B
t�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
x�B
x�B
x�B
zB
y�B
y�B
y�B
y�B
y�B
z�B
{B
{B
{0B
|B
|B
}B
}B
}B
~B
|�B
~B
}"B
~B
~B
B
~�B
B
~�B
~�B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612180037322016121800373220161218003732201806221306222018062213062220180622130622201804050706382018040507063820180405070638  JA  ARFMdecpA19c                                                                20161214063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161213213518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161213213519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161213213522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161213213523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161213213523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161213213523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161213213523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161213213523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161213213524                      G�O�G�O�G�O�                JA  ARUP                                                                        20161213223749                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161214014209  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161217153732  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161217153732  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220638  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040622  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                