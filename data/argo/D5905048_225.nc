CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-30T00:35:32Z creation;2018-03-30T00:35:36Z conversion to V3.1;2019-12-19T07:41:55Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180330003532  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_225                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�W5B�\ 1   @�W6�[ @4Ǡ�	k��dEL�_�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�HCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!�RD"~�D"��D#~�D#��D$xRD$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�L)D�b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��
A���A�ȴA�ĜA�AɾwAɼjAɼjAɶFAɲ-Aɧ�Aɟ�Aə�AɑhAɍPA�t�A�l�A�ZA�VA�G�A�%Aȧ�A� �AǼjA�hsA�XA���A�A�A�1'A���Aź^AōPAŁA�`BA��#A�5?A�K�A�$�A�A�n�A�G�A��A���A�VA���A�{A��A�&�A���A���A��A�A�A�~�A��A��A��!A��A���A��A�K�A�$�A��A���A���A��A�A�hsA��A�S�A���A�"�A�`BA��!A���A���A��TA�bNA��mA��A��A���A���A�  A�9XA�&�A�Q�A��RA��A�r�A�jA�/A��yA�v�A��RA���A�+A��A��uA��
A���A�"�A�S�A���A�ĜA�z�A��A�9XA�ffA��A��A�^5A�7LA�^5A�n�A��jA���A\)A}�wA}`BA|��A{�;Az�!Ax�`Av��As�-Ao�An��An�AlM�Akl�Ajr�Ai/Af��AehsAa��A_p�A\n�A[�AZE�AY�hAX�AX��AX�\AXbNAW��AU;dAQO�AP�uAPv�AO+AN��AN�DAM�ALAKS�AJffAG�;AFZAEK�ADJACO�AB�A@z�A?
=A=O�A< �A9��A7oA5hsA4M�A3\)A2ffA1C�A//A-%A,�RA,�+A,VA+��A+VA(�jA&JA$�uA#C�A"A�A!
=A�;A?}A
=A(�A�A%A9XA��A�Az�A�#Al�A�AjA��A�DAoAI�At�Ar�A1'AoA?}AAp�A"�A
�!A	��A	G�A��A(�A��A��A��AoA��AXA�uAS�@��;@���@�l�@���@��h@�=q@�O�@�A�@�E�@���@�p�@�1@�dZ@��y@�+@��T@���@�1'@�t�@��H@�{@�O�@��@�bN@�9X@��;@��y@��#@�1@�t�@�\)@��@�M�@ᙚ@��D@�K�@�=q@݁@܃@�S�@���@�1@��y@��@ղ-@�O�@��`@�ƨ@�S�@Ұ!@���@�1'@Ͼw@υ@�33@�=q@ͺ^@�p�@˥�@ɑh@Ǯ@�
=@��y@ũ�@�C�@��7@���@�(�@�9X@��H@�=q@�hs@��@��P@�K�@��@���@��\@�-@���@�&�@���@�1@���@�dZ@�ȴ@�E�@��T@���@�X@���@�b@��@�K�@�
=@���@�^5@�J@��@��9@�1'@��
@��P@�o@�5?@�-@�J@�@���@��@�  @��
@�dZ@��@�
=@��@��H@�ȴ@�V@��@�+@���@��\@���@���@��!@���@�M�@��@���@��@�Ĝ@���@��@���@���@�K�@�S�@�S�@�C�@�dZ@�dZ@���@��R@�M�@��-@�x�@�p�@�&�@��`@��@�A�@��
@��w@��@���@���@�\)@���@��R@��!@��@��@��@���@��+@��\@��+@�~�@�ff@�-@��T@���@��-@�hs@�V@���@�Ĝ@�A�@��F@��@�l�@�S�@���@��!@���@��\@�ff@�$�@���@��@���@�`B@���@���@��j@��@���@��@�Z@�1@��;@��w@���@��@��y@��H@�ȴ@���@���@�n�@�V@�=q@��@���@�hs@�7L@���@���@�Z@�9X@� �@���@�o@���@�ff@�$�@��@��^@�p�@�`B@�7L@���@��`@���@��9@���@�(�@��;@���@�33@��y@��\@�v�@�V@���@��+@�ff@�=q@��@��@���@��-@��7@�`B@��@��`@���@�z�@�I�@��@��
@�ƨ@���@��@�dZ@�\)@�S�@�C�@�33@�o@��y@���@�~�@�M�@��@�O�@��@��@��9@��u@�r�@�(�@��P@�ȴ@�^5@�M�@�V@�-@�{@��@���@�p�@�7L@��@���@���@��@�j@�I�@�9X@�1'@�(�@�b@��@��;@��
@�ƨ@���@�t�@�;d@�"�@���@��\@�^5@�=q@�-@��@��T@���@��@��`@���@�Q�@�1'@�  @|�@\)@;d@~�y@~��@~ff@}�-@|�@|(�@{��@{o@z��@y��@yhs@x�@w��@wl�@v��@v��@vE�@u�-@uV@tZ@t1@s�F@sC�@r��@r=q@q�#@qX@p�@pA�@o�@oK�@n��@nE�@m�@m@m�-@m�h@m?}@l�/@l�D@lZ@l9X@l1@k��@k�
@kƨ@k�F@kdZ@k"�@j��@j�!@j�\@jn�@jJ@i��@i��@i��@i��@i&�@h��@hr�@h �@g��@g;d@fȴ@f$�@eO�@d�@dz�@d9X@c�m@c�F@c��@c��@c��@c�@cdZ@c33@b��@b�\@b~�@bM�@a�#@a7L@`��@`�9@`Q�@`b@_�w@_\)@_�@^��@^ȴ@^v�@^$�@]�@]�@\��@\z�@\Z@\I�@\�@[ƨ@[dZ@Z�@Z�!@Z�@Yx�@X��@XĜ@X�u@XQ�@W|�@V��@Vff@VE�@V{@U��@U��@U�h@U`B@UV@TZ@SS�@R=q@Qx�@P�`@P�u@Pr�@P  @O;d@N��@N��@N5?@N@N@M�T@M�h@M�@L��@L��@L��@Lz�@LZ@LZ@LZ@L9X@K�
@K��@KS�@KC�@K@J��@J��@J�\@J~�@Jn�@J=q@JJ@I��@I�@I�#@I�^@I��@Ihs@I&�@I�@I%@H��@Hr�@H �@G�w@F��@F$�@F@E�@E�h@E`B@EO�@E/@EV@D��@D�@D9X@C�m@C�@CC�@B�H@BJ@A�@A�7@Ahs@@��@@�u@@r�@?�@?��@?;d@>5?@=��@=`B@=O�@=V@<��@<z�@<�@;ƨ@;��@;C�@:�@:�\@:=q@9�#@9hs@8�9@8 �@8b@7��@7;d@7�@6�@6E�@5�T@5��@5��@5��@5�-@5V@4�@4�@4��@4j@4�@3ƨ@3��@3S�@3"�@3@2�@2��@2�\@2=q@1�@1�#@1�#@1�#@1�^@1��@1�7@1X@1%@0�`@0�9@0�u@0A�@/�@/�@/|�@/+@.$�@-�T@-�T@-��@-�-@-��@-�h@-�@-�@-p�@-p�@-�@-?}@,��@,��@,Z@+�m@+�F@+t�@+C�@*�@*=q@)�^@)�7@)x�@)G�@)%@(��@(�9@(�9@(��@(�u@(bN@(1'@(  @'�w@'�P@'K�@'K�@';d@'+@&ȴ@&v�@&V@&$�@&@%�@%�-@%O�@$��@$�/@$�j@$�D@$j@#��@#ƨ@#��@#t�@"�@"�H@"��@"~�@"=q@!��@!hs@!7L@ ��@ Ĝ@ �9@ �9@ ��@ �@ Q�@   @�;@�w@�w@��@l�@�@�@��@5?@��@@�-@��@�@`B@V@�j@z�@I�@(�@�@��@��@t�@C�@o@��@�@��@��@�7@hs@&�@��@�@��@l�@;d@��@ȴ@�R@�+@{@��@�-@O�@/@V@��@�@�@�@�/@��@�j@�@��@��@z�@1@ƨ@ƨ@ƨ@�F@�@S�@�H@��@~�@n�@n�@n�@M�@-@J@��@�@��@�@��@�^@��@�^@��@hs@7L@7L@7L@&�@%@�u@A�@b@�@�;@�P@K�@K�@K�@;d@;d@;d@;d@�@
=@�y@ȴ@�+@ff@5?@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��
A���A�ȴA�ĜA�AɾwAɼjAɼjAɶFAɲ-Aɧ�Aɟ�Aə�AɑhAɍPA�t�A�l�A�ZA�VA�G�A�%Aȧ�A� �AǼjA�hsA�XA���A�A�A�1'A���Aź^AōPAŁA�`BA��#A�5?A�K�A�$�A�A�n�A�G�A��A���A�VA���A�{A��A�&�A���A���A��A�A�A�~�A��A��A��!A��A���A��A�K�A�$�A��A���A���A��A�A�hsA��A�S�A���A�"�A�`BA��!A���A���A��TA�bNA��mA��A��A���A���A�  A�9XA�&�A�Q�A��RA��A�r�A�jA�/A��yA�v�A��RA���A�+A��A��uA��
A���A�"�A�S�A���A�ĜA�z�A��A�9XA�ffA��A��A�^5A�7LA�^5A�n�A��jA���A\)A}�wA}`BA|��A{�;Az�!Ax�`Av��As�-Ao�An��An�AlM�Akl�Ajr�Ai/Af��AehsAa��A_p�A\n�A[�AZE�AY�hAX�AX��AX�\AXbNAW��AU;dAQO�AP�uAPv�AO+AN��AN�DAM�ALAKS�AJffAG�;AFZAEK�ADJACO�AB�A@z�A?
=A=O�A< �A9��A7oA5hsA4M�A3\)A2ffA1C�A//A-%A,�RA,�+A,VA+��A+VA(�jA&JA$�uA#C�A"A�A!
=A�;A?}A
=A(�A�A%A9XA��A�Az�A�#Al�A�AjA��A�DAoAI�At�Ar�A1'AoA?}AAp�A"�A
�!A	��A	G�A��A(�A��A��A��AoA��AXA�uAS�@��;@���@�l�@���@��h@�=q@�O�@�A�@�E�@���@�p�@�1@�dZ@��y@�+@��T@���@�1'@�t�@��H@�{@�O�@��@�bN@�9X@��;@��y@��#@�1@�t�@�\)@��@�M�@ᙚ@��D@�K�@�=q@݁@܃@�S�@���@�1@��y@��@ղ-@�O�@��`@�ƨ@�S�@Ұ!@���@�1'@Ͼw@υ@�33@�=q@ͺ^@�p�@˥�@ɑh@Ǯ@�
=@��y@ũ�@�C�@��7@���@�(�@�9X@��H@�=q@�hs@��@��P@�K�@��@���@��\@�-@���@�&�@���@�1@���@�dZ@�ȴ@�E�@��T@���@�X@���@�b@��@�K�@�
=@���@�^5@�J@��@��9@�1'@��
@��P@�o@�5?@�-@�J@�@���@��@�  @��
@�dZ@��@�
=@��@��H@�ȴ@�V@��@�+@���@��\@���@���@��!@���@�M�@��@���@��@�Ĝ@���@��@���@���@�K�@�S�@�S�@�C�@�dZ@�dZ@���@��R@�M�@��-@�x�@�p�@�&�@��`@��@�A�@��
@��w@��@���@���@�\)@���@��R@��!@��@��@��@���@��+@��\@��+@�~�@�ff@�-@��T@���@��-@�hs@�V@���@�Ĝ@�A�@��F@��@�l�@�S�@���@��!@���@��\@�ff@�$�@���@��@���@�`B@���@���@��j@��@���@��@�Z@�1@��;@��w@���@��@��y@��H@�ȴ@���@���@�n�@�V@�=q@��@���@�hs@�7L@���@���@�Z@�9X@� �@���@�o@���@�ff@�$�@��@��^@�p�@�`B@�7L@���@��`@���@��9@���@�(�@��;@���@�33@��y@��\@�v�@�V@���@��+@�ff@�=q@��@��@���@��-@��7@�`B@��@��`@���@�z�@�I�@��@��
@�ƨ@���@��@�dZ@�\)@�S�@�C�@�33@�o@��y@���@�~�@�M�@��@�O�@��@��@��9@��u@�r�@�(�@��P@�ȴ@�^5@�M�@�V@�-@�{@��@���@�p�@�7L@��@���@���@��@�j@�I�@�9X@�1'@�(�@�b@��@��;@��
@�ƨ@���@�t�@�;d@�"�@���@��\@�^5@�=q@�-@��@��T@���@��@��`@���@�Q�@�1'@�  @|�@\)@;d@~�y@~��@~ff@}�-@|�@|(�@{��@{o@z��@y��@yhs@x�@w��@wl�@v��@v��@vE�@u�-@uV@tZ@t1@s�F@sC�@r��@r=q@q�#@qX@p�@pA�@o�@oK�@n��@nE�@m�@m@m�-@m�h@m?}@l�/@l�D@lZ@l9X@l1@k��@k�
@kƨ@k�F@kdZ@k"�@j��@j�!@j�\@jn�@jJ@i��@i��@i��@i��@i&�@h��@hr�@h �@g��@g;d@fȴ@f$�@eO�@d�@dz�@d9X@c�m@c�F@c��@c��@c��@c�@cdZ@c33@b��@b�\@b~�@bM�@a�#@a7L@`��@`�9@`Q�@`b@_�w@_\)@_�@^��@^ȴ@^v�@^$�@]�@]�@\��@\z�@\Z@\I�@\�@[ƨ@[dZ@Z�@Z�!@Z�@Yx�@X��@XĜ@X�u@XQ�@W|�@V��@Vff@VE�@V{@U��@U��@U�h@U`B@UV@TZ@SS�@R=q@Qx�@P�`@P�u@Pr�@P  @O;d@N��@N��@N5?@N@N@M�T@M�h@M�@L��@L��@L��@Lz�@LZ@LZ@LZ@L9X@K�
@K��@KS�@KC�@K@J��@J��@J�\@J~�@Jn�@J=q@JJ@I��@I�@I�#@I�^@I��@Ihs@I&�@I�@I%@H��@Hr�@H �@G�w@F��@F$�@F@E�@E�h@E`B@EO�@E/@EV@D��@D�@D9X@C�m@C�@CC�@B�H@BJ@A�@A�7@Ahs@@��@@�u@@r�@?�@?��@?;d@>5?@=��@=`B@=O�@=V@<��@<z�@<�@;ƨ@;��@;C�@:�@:�\@:=q@9�#@9hs@8�9@8 �@8b@7��@7;d@7�@6�@6E�@5�T@5��@5��@5��@5�-@5V@4�@4�@4��@4j@4�@3ƨ@3��@3S�@3"�@3@2�@2��@2�\@2=q@1�@1�#@1�#@1�#@1�^@1��@1�7@1X@1%@0�`@0�9@0�u@0A�@/�@/�@/|�@/+@.$�@-�T@-�T@-��@-�-@-��@-�h@-�@-�@-p�@-p�@-�@-?}@,��@,��@,Z@+�m@+�F@+t�@+C�@*�@*=q@)�^@)�7@)x�@)G�@)%@(��@(�9@(�9@(��@(�u@(bN@(1'@(  @'�w@'�P@'K�@'K�@';d@'+@&ȴ@&v�@&V@&$�@&@%�@%�-@%O�@$��@$�/@$�j@$�D@$j@#��@#ƨ@#��@#t�@"�@"�H@"��@"~�@"=q@!��@!hs@!7L@ ��@ Ĝ@ �9@ �9@ ��@ �@ Q�@   @�;@�w@�w@��@l�@�@�@��@5?@��@@�-@��@�@`B@V@�j@z�@I�@(�@�@��@��@t�@C�@o@��@�@��@��@�7@hs@&�@��@�@��@l�@;d@��@ȴ@�R@�+@{@��@�-@O�@/@V@��@�@�@�@�/@��@�j@�@��@��@z�@1@ƨ@ƨ@ƨ@�F@�@S�@�H@��@~�@n�@n�@n�@M�@-@J@��@�@��@�@��@�^@��@�^@��@hs@7L@7L@7L@&�@%@�u@A�@b@�@�;@�P@K�@K�@K�@;d@;d@;d@;d@�@
=@�y@ȴ@�+@ff@5?@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
e`B
ffB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
e`B
e`B
dZB
e`B
ffB
gmB
iyB
m�B
n�B
u�B
u�B
x�B
x�B
y�B
�bB
��B
��B
��B%�B/B]/B�%B�%B�bB�uB��B��B�oB��B�-B�B��BPB�B�B\BB%B�B-B1'B:^B1'B.B,B9XB@�BJ�BVBQ�BJ�BH�B`BBs�Bo�Bp�Be`B]/BgmBiyBffBZBQ�B0!B�B	7B\B��B  B��B%B��B��B�yB�HB�B�#B��B��B��BǮBÖB�9B�dB�FB��B��B�\B�BXBP�BK�BZBXBM�BE�B1'BbB1BB
�B
�B
�VB
�{B
� B
E�B
7LB
�B
+B
�B
�B
)�B
�B
JB	��B	�sB	��B	�wB	��B	�}B	ŢB	�9B	�?B	�B	��B	�B	y�B	N�B	Q�B	?}B	M�B	N�B	M�B	K�B	N�B	F�B	?}B	,B	PB�NB	VB	�B	B	+B	B�B�B�B�HB��B��B�B��B��BĜB�wB�?B�B��B��B�PB��B��B��B��B��B�\B�1B��B��B��B�hB�Bk�BaHBk�Bt�By�Bv�Bt�B�B�B|�By�B� Bx�Bz�Bu�B�By�Bx�Bw�Bo�BgmB`BB[#B_;B^5BS�BE�BN�BI�BQ�BaHBaHB_;BVB\)BW
BZBXBQ�BT�BVB\)BP�BO�BL�BF�BG�BR�BW
BT�BF�BVBT�BQ�B[#B]/BT�B]/B\)B]/B[#BXBYBYBYBXBW
B\)B`BBaHB_;B_;BaHB`BBl�Bu�Br�Bm�Bl�Bm�Bn�Bs�Bs�Bs�Bn�Bp�Bn�B{�B� B�1B�+B�B~�B�B�B}�B�7B�\B�hB�\B�JB�VB�\B�B�B�1B�uB��B�VB�+B�PB��B��B��B��B�B��B��B�LB�XB�^B�jB�dB�^B�dB�dB�}BÖBŢBɺBǮB��B��B��B��B��B��B�#B�BB�TB�`B�yB�yB�sB�B�B��B��B��B��B	DB	PB	\B	\B	oB	�B	�B	�B	!�B	$�B	$�B	$�B	(�B	+B	+B	'�B	;dB	G�B	L�B	M�B	M�B	M�B	N�B	P�B	P�B	K�B	Q�B	S�B	R�B	P�B	S�B	W
B	\)B	\)B	\)B	^5B	]/B	ZB	aHB	m�B	q�B	z�B	�B	�B	�B	�B	�B	�+B	�PB	�VB	�\B	�\B	�\B	�VB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�LB	�LB	�FB	�LB	�^B	�dB	�XB	�^B	��B	B	ÖB	��B	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�
B	�B	�B	�#B	�5B	�5B	�5B	�;B	�5B	�;B	�BB	�;B	�;B	�BB	�TB	�ZB	�ZB	�`B	�mB	�fB	�ZB	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
+B
+B
+B
+B
%B
+B
+B
B
B

=B
JB
JB
PB
PB
JB

=B

=B
\B
uB
{B
uB
uB
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
!�B
!�B
"�B
$�B
$�B
#�B
#�B
$�B
$�B
#�B
"�B
%�B
$�B
%�B
$�B
&�B
'�B
(�B
(�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
+B
)�B
+B
)�B
(�B
)�B
)�B
+B
+B
+B
)�B
+B
,B
,B
+B
)�B
)�B
+B
)�B
)�B
)�B
)�B
(�B
(�B
+B
-B
.B
-B
/B
/B
0!B
/B
/B
/B
.B
.B
/B
/B
.B
-B
,B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
0!B
/B
2-B
33B
33B
33B
2-B
2-B
2-B
2-B
1'B
2-B
33B
5?B
5?B
49B
2-B
33B
7LB
8RB
7LB
7LB
8RB
8RB
7LB
6FB
49B
33B
49B
7LB
9XB
:^B
;dB
:^B
9XB
<jB
<jB
<jB
>wB
?}B
>wB
=qB
=qB
?}B
@�B
?}B
?}B
@�B
A�B
A�B
@�B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
C�B
C�B
D�B
D�B
C�B
A�B
B�B
A�B
?}B
C�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
F�B
E�B
F�B
F�B
G�B
F�B
E�B
I�B
H�B
I�B
H�B
H�B
J�B
H�B
I�B
I�B
G�B
J�B
L�B
M�B
L�B
L�B
L�B
L�B
M�B
N�B
M�B
M�B
M�B
N�B
M�B
N�B
M�B
O�B
Q�B
P�B
O�B
Q�B
Q�B
P�B
R�B
T�B
T�B
S�B
R�B
Q�B
T�B
T�B
S�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
W
B
W
B
XB
XB
XB
W
B
XB
XB
XB
W
B
T�B
ZB
[#B
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
[#B
ZB
YB
[#B
ZB
YB
\)B
[#B
\)B
[#B
YB
\)B
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
^5B
_;B
^5B
_;B
_;B
aHB
aHB
`BB
_;B
`BB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
aHB
bNB
cTB
cTB
aHB
dZB
dZB
cTB
cTB
bNB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
ffB
ffB
ffB
gmB
ffB
gmB
iyB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
jB
jB
jB
jB
iyB
jB
jB
jB
iyB
iyB
jB
l�B
k�B
l�B
k�B
k�B
jB
iyB
k�B
l�B
l�B
m�B
m�B
l�B
k�B
l�B
m�B
m�B
n�B
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
p�B
o�B
n�B
p�B
q�B
q�B
q�B
p�B
p�B
o�B
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
t�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ezB
ffB
d�B
ezB
ffB
f�B
ffB
f�B
ffB
e`B
ezB
dtB
ezB
f�B
g�B
i�B
m�B
n�B
u�B
u�B
x�B
y$B
z�B
�NB
�B
ѷB
�B%�B0�B^B�B��B��B��B��B�
B��B�5B��B�B�}BVB�BEB.BBYBB,�B1[B;B3hB0;B-�B:�BB'BK�BV�BS&BK�BJXBabBs�Bp;BqABg8B_Bh$BjeBg�B[�BS�B3�B�B�BB��BAB��B_B �B�B�B�TB�WB��B�BBңB�BB�7B�B�FB�B�B�0B�yB��B��B]�BU�BO�B\]BY�BO�BGzB3�B�B
=B�B
�cG�O�B
�TB
��B
��B
JXB
;�B
�B
�B
!�B
CB
*eB
 vB
�B	��B	��B	��B	�AB	�B	��B	ƨB	�`B	�zB	�}B	��B	�9B	}"G�O�B	T�B	B�B	OBB	O�B	N�B	L~B	O(B	G_B	@�G�O�G�O�B��B	B	mG�O�B	�B	�B��B��B�B�B��BοB�sB�}B�B�tB��B�LB�iB�B��B�}B��B�LB�8B�NB��B�B��B�B�B�#B��B�EG�O�Bd�BmwBvzB{JBxlBvFB��B��B~Bz�B��BzB{�Bv�B�UBz�By�Bx�Bp�Bh�Ba�B\�B`vB_�BU�BH�BP�BLBS�Ba�Ba�B`BWYB\�BXBZ�BX�BS[BV9BW$B\�BR�BQ4BN�BH�BI�BS�BW�BVBH�BV�BVBS&B[�B]�BVB]�B\�B]�B[�BX�BY�BY�BY�BX�BW�B\�B`�Ba|B_�B`Bb4Ba|Bl�Bu�Bs3Bn/BmCBn}Bo�Bt�BtnBt�Bo�Bq�Bo�B|�B��B�fB��B��B�B�{B��B.B��B��B��B��B�B��B��B��B��B��B��B�B�vB��B��B�/B�:B�0B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B��B�1B�0B�4B�:B�@B�oBՁB�qB��B�B�B�B��B�*B�B�B�B�*B�JB��B	DB	�B	�B	�B	B	�B	�B	B	!�B	$�B	$�B	%B	)DB	+�B	,"B	)B	;�B	G�B	L�B	M�B	M�B	NB	O(B	Q4B	Q4B	L~B	RB	T,B	S@B	QNB	TFB	W?B	\)B	\CB	\CB	^5B	]IB	Z�B	abB	m�B	rB	z�B	� B	�AB	�AB	�MB	�gB	�zB	�jB	�pB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�;B	�GB	��B	�tB	�fB	�fB	��B	��B	��B	�B	��B	��B	��B	ªB	ðB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�B	�,B	�B	�9B	�B	�MB	�FB	�B	�$B	�EB	�mB	�=B	�5B	�OB	�OB	�VB	�jB	�pB	�\B	�pB	ߊB	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�B	�B	�B	�BB	�(B	�.B	�.B
 B
;B
;B
3B
3B
3B
9B
?B
EB
KB
_B
EB
EB
_B
YB
EB
_B
mB
�B

XB
dB
�B
�B
�B
�B

�B

�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
B
 B
 B
 B
!�B
#B
#B
#B
!�B
"B
# B
$�B
$�B
$B
$B
%B
%B
$B
# B
&B
%B
&B
%B
'B
($B
)B
)B
(
B
(
B
($B
(
B
*B
*B
*B
+B
*0B
+B
*B
)*B
*B
*B
+B
+6B
+B
*0B
+B
,B
,B
+B
*0B
*0B
+6B
*0B
*0B
*0B
*0B
)_B
)DB
+6B
-)B
./B
-)B
/OB
/B
0;B
/OB
/5B
/5B
./B
.IB
/5B
/5B
./B
-CB
,WB
/5B
/5B
/OB
0;B
0;B
0;B
1AB
1AB
1AB
0;B
0;B
1AB
0UB
/iB
2GB
3MB
3MB
3hB
2aB
2aB
2aB
2GB
1[B
2aB
3hB
5ZB
5ZB
4nG�O�B
3�B
7fB
8�B
7fB
7fB
8�B
8�B
7fB
6zG�O�B
3�B
4�B
7�B
9�B
:xB
;B
:�B
9�B
<�B
<�B
<�B
>�B
?}B
>�B
=�B
=�B
?�B
@�B
?�B
?�B
@�B
A�B
A�B
@�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
C�B
C�B
D�B
D�B
C�G�O�B
B�B
A�B
?�B
C�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
F�B
E�B
F�B
F�B
G�B
F�B
E�B
I�B
H�B
I�B
H�B
H�B
J�G�O�B
I�B
J	G�O�B
J�B
L�B
M�B
L�B
L�B
L�B
MB
M�B
N�B
N"B
M�B
NB
N�B
NB
OB
N"B
PB
RB
Q B
PB
RB
R B
Q4B
S&B
T�B
T�B
S�B
SB
R B
T�B
T�B
TB
S&B
TB
TB
U2B
UB
VB
VB
VB
VB
VB
VB
W$B
X+B
XB
XB
XEB
X+B
X+B
W$B
W$B
X+B
XEB
XEB
W?B
X+B
X+B
X+B
WYG�O�B
ZB
[#B
[#B
[WB
[#B
\)B
[=B
\)B
\)B
\)B
[#B
Z7B
YKB
[#B
ZQB
YKB
\CB
[=B
\CB
[WG�O�B
\]B
]IB
^OB
^OB
^OB
^OB
_;B
_;B
_;B
_pB
_VB
^OB
_VB
^jB
_VB
_VB
aHB
aHB
`\B
_pB
`\B
abB
a|B
a|B
abB
`\B
`vB
a|B
bhB
bhB
bhB
b�B
a|B
bhB
c�B
cnG�O�B
dtB
dtB
cnB
cnB
b�B
d�B
ezB
ezB
ezB
ffB
ffB
ffB
f�B
ezB
ezB
g�B
g�B
gmB
g�B
f�B
f�B
f�B
g�B
f�B
g�B
i�B
iyB
iyB
i�B
i�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
i�B
j�B
j�B
j�B
i�B
i�B
j�B
l�B
k�B
l�B
k�B
k�B
j�B
i�B
k�B
l�B
l�B
m�B
m�B
l�B
k�B
l�B
m�B
m�B
n�B
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
p�B
o�B
n�B
p�B
q�B
q�B
q�B
p�B
p�B
o�B
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
t�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111141111111114411141111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111411111111111111111111111111111111111111111111114111111111111111111111111141141111111111111111111111111111111111111111111111111111111111114111111111111111111114111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804030043382018040300433820180403004338201806221328212018062213282120180622132821201804261706462018042617064620180426170646  JA  ARFMdecpA19c                                                                20180330093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180330003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180330003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180330003535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180330003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180330003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180330003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180330003536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180330003536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180330003536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180330010232                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180330153456  CV  JULD            G�O�G�O�F¹�                JM  ARSQJMQC2.0                                                                 20180402000000  CF  PSAL_ADJUSTED_QCC^  Dڀ G�O�                JM  ARCAJMQC2.0                                                                 20180402154338  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180402154338  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426080646  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042821  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                