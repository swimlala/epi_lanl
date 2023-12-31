CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-26T21:35:13Z creation;2017-11-26T21:35:17Z conversion to V3.1;2019-12-19T07:51:38Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171126213513  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_184                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�8sr(3�1   @�8t333 @4XbM��d��n.�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�\)@�(�A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C"{C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D�RD~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,xRD,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\DҼ)D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�AԬAԮAԮAԮA԰!A԰!A԰!A԰!AԴ9AԴ9AԶFAԴ9AԴ9AԴ9AԶFAԶFAԸRAԸRAԸRAԺ^AԺ^AԺ^AԼjAԶFAԴ9AԴ9A԰!A԰!Aԩ�Aԡ�A��A΋DA���Aĉ7A�(�A� �A��mA�G�A��^A���A��A���A�
=A��uA�|�A�v�A�z�A�E�A��A�
=A�E�A���A��!A��
A�ĜA��A�$�A�XA�-A�VA�XA�I�A�9XA�jA��FA�XA�jA�"�A�$�A�ZA�dZA�n�A��A�{A�M�A�{A��hA�l�A�$�A���A�A�33A�bA�oA���A��A���A�"�A��jA��A�hsA��A��RA��A�5?A�r�A�;dA�=qA�"�A���A�E�A�K�A�VA~{A}S�A|r�Az�AxE�Av��As�Ar�AqƨAo�Al(�Ah��Af9XAdJAa�Aa%A_�A]�PA\ȴA[��A[G�AZ�AY�-AXA�AW�AV��AU%AQ�#AO/ANQ�AMdZAK��AK�AJz�AIl�AH�!AGl�AF9XAEt�AD�AB�`AA�FA?�mA=�A:ȴA89XA7�A6�A4^5A2=qA/l�A.ȴA-��A,I�A)�#A(��A'�A%A%\)A%\)A$z�A$bA#�FA#;dA"Q�A!�FA �A   A{A�A�A$�A"�A�DA1'A�A�PA��A�A�jA�^A"�A�PA-A33AVAS�AbNA��A
I�A	`BA	G�A�`A{A\)AA�+A{At�A�A�;A\)A%A �A��At�A?}A �/A Z@���@��y@�@���@���@���@�5?@���@�ff@���@��m@�M�@��/@@�hs@��
@�ȴ@�J@�@���@�j@�F@�V@�@��@�  @���@�E�@��@��@܋D@ۅ@�;d@ڧ�@ٺ^@�1'@�S�@��@ԣ�@җ�@�X@�9X@�t�@�ȴ@��@̛�@˅@ʰ!@��#@��@�1@�|�@�K�@�^5@�O�@�%@Ĭ@�1'@�ƨ@��@�E�@���@�V@�Z@���@��@���@��/@�Z@�A�@��m@��R@���@���@�Ĝ@�  @�S�@��!@�J@�G�@�Ĝ@�I�@���@��@�S�@��y@�~�@�{@�%@��@��`@���@�1'@�l�@�;d@��@��@�M�@���@�x�@�/@��@��@�bN@�I�@�(�@��@�1@��w@���@�t�@�+@�ȴ@��+@�^5@���@�V@�7L@���@�"�@��y@���@���@���@��h@�&�@���@��j@��
@���@�t�@�S�@�+@��R@�v�@�=q@��T@��@�p�@�O�@���@���@�Z@��@���@�S�@�o@�@��@���@�@��h@�O�@��@��@�%@���@��`@���@���@��@���@�@�E�@��\@�v�@��/@��D@��`@��9@��@��@�Z@�(�@�  @��@���@��@�K�@��@�l�@���@�K�@�o@��@�ȴ@��R@�5?@��@�$�@�$�@�@��^@��h@�G�@��@���@��D@�Z@�Q�@�I�@�(�@�b@���@��m@��w@�l�@�"�@�
=@�@���@��y@��R@��!@�M�@��-@�x�@��@�V@���@��`@���@��@���@��D@���@�r�@�Q�@�(�@�S�@�
=@��@��H@���@�n�@�{@��#@�@��^@���@�`B@�%@�z�@��m@���@�S�@�33@��@���@�v�@��@���@���@��h@��7@�`B@�V@���@�Z@�9X@�b@��@�ƨ@��@���@���@���@���@�dZ@��@�ȴ@��+@�v�@�n�@�~�@�ff@�ff@�-@�-@��@��@�V@��@�Z@�Q�@�j@�Q�@��F@�+@�
=@�@��H@��@���@�V@�E�@�5?@�J@��#@��-@�hs@��`@���@�r�@�j@�j@�1@���@�\)@�"�@���@�v�@�v�@�^5@�5?@���@���@���@�hs@�O�@�V@��9@+@~��@~E�@}�@}?}@}�@|�/@|�D@|�@{t�@{@z��@z��@zn�@z�@y��@y�7@yhs@yG�@y7L@y7L@x�`@x1'@w�@w��@w;d@v�@v�R@vff@u@t�@s��@s�@st�@sdZ@s33@r�@r��@rn�@q�#@qX@pĜ@pA�@o�w@o|�@o;d@nȴ@n�+@n$�@m��@m�h@m`B@m�@l�j@l�@k�m@kS�@j�!@j~�@i��@h��@hĜ@h��@hQ�@h �@g�@g�w@g�P@g+@f��@fE�@e�@e��@e�-@e�h@eO�@e�@d�/@dz�@d1@c��@c��@cS�@c33@co@b�H@b=q@a��@ax�@a%@`��@`1'@_�@_�@^�@^��@^v�@^5?@]�@]��@]`B@]/@\�@\�D@\Z@\I�@\1@[��@[dZ@Z�H@Z��@Z��@Z��@Z~�@Z-@ZJ@Y�@Y�#@Y��@Y��@Y�7@Yx�@X��@XA�@X �@W�@W�w@W\)@W+@W+@W�@V��@V�@V��@VE�@V{@U��@U`B@T�/@Tz�@T(�@T1@S��@S�m@S33@R�\@R-@Q��@QX@Q%@P�u@Pb@O��@Ol�@N��@NV@M@MO�@M/@L�@L��@L�@L9X@Kƨ@KS�@K"�@K33@K@J��@JJ@I��@Ix�@I&�@H�`@H��@HbN@G+@F�+@F5?@F@E`B@E�@D��@Dj@D9X@C��@Cƨ@C��@Ct�@Co@B��@B�\@Bn�@BM�@B=q@B=q@B-@A��@@�9@@r�@@ �@@  @?��@?l�@?+@>��@=��@=`B@=�@<��@<��@<�@<�@<��@<Z@;dZ@;@:�H@:��@:��@:�\@:~�@:~�@:M�@:M�@:=q@:=q@:�@9�#@9�^@9x�@8bN@8b@7�@7�;@7�w@7K�@7K�@7�@6ȴ@6�+@6V@6$�@6{@5�T@5�h@5`B@5�@4��@4Z@4�@41@3��@3C�@333@3@2�!@2M�@2J@1��@1�#@1��@1X@0��@0��@0Q�@0b@0b@0  @/�w@/\)@/�@.�R@.�+@-�@-�@-�@-p�@,��@,��@,j@,I�@,(�@+�
@+"�@*�H@*��@*�\@*^5@*=q@*J@)�^@)�7@)X@)7L@)%@(Ĝ@(��@(Q�@(A�@(  @'�@'�P@'K�@'
=@&�y@&��@&$�@%�-@%�h@%p�@%p�@%O�@%/@$��@$��@$z�@$I�@$�@#�
@#��@#dZ@#S�@#S�@#33@#"�@"��@"��@"��@"~�@"=q@"-@!�@!��@!��@!�7@!hs@!7L@ �`@ 1'@   @�;@�@��@�P@|�@\)@K�@K�@;d@+@
=@�@��@�+@V@�T@�-@��@�h@O�@/@�@��@�j@�@�D@z�@j@z�@j@I�@1@�m@ƨ@��@��@��@t�@dZ@C�@�@��@��@��@M�@�@�@�#@��@��@��@�7@��@�@bN@ �@��@+@+@�@
=@�y@�+@V@@��@�-@�@p�@p�@O�@?}@V@�/@��@�j@��@I�@1@ƨ@��@33@�H@�!@�\@n�@M�@�#@��@hs@7L@&�@�@��@Ĝ@�9@��@�@  @�;@�w@\)@;d@+@�@�@�R@�R@��@v�@5?@��@�h@O�@V@��@Z@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�AԬAԮAԮAԮA԰!A԰!A԰!A԰!AԴ9AԴ9AԶFAԴ9AԴ9AԴ9AԶFAԶFAԸRAԸRAԸRAԺ^AԺ^AԺ^AԼjAԶFAԴ9AԴ9A԰!A԰!Aԩ�Aԡ�A��A΋DA���Aĉ7A�(�A� �A��mA�G�A��^A���A��A���A�
=A��uA�|�A�v�A�z�A�E�A��A�
=A�E�A���A��!A��
A�ĜA��A�$�A�XA�-A�VA�XA�I�A�9XA�jA��FA�XA�jA�"�A�$�A�ZA�dZA�n�A��A�{A�M�A�{A��hA�l�A�$�A���A�A�33A�bA�oA���A��A���A�"�A��jA��A�hsA��A��RA��A�5?A�r�A�;dA�=qA�"�A���A�E�A�K�A�VA~{A}S�A|r�Az�AxE�Av��As�Ar�AqƨAo�Al(�Ah��Af9XAdJAa�Aa%A_�A]�PA\ȴA[��A[G�AZ�AY�-AXA�AW�AV��AU%AQ�#AO/ANQ�AMdZAK��AK�AJz�AIl�AH�!AGl�AF9XAEt�AD�AB�`AA�FA?�mA=�A:ȴA89XA7�A6�A4^5A2=qA/l�A.ȴA-��A,I�A)�#A(��A'�A%A%\)A%\)A$z�A$bA#�FA#;dA"Q�A!�FA �A   A{A�A�A$�A"�A�DA1'A�A�PA��A�A�jA�^A"�A�PA-A33AVAS�AbNA��A
I�A	`BA	G�A�`A{A\)AA�+A{At�A�A�;A\)A%A �A��At�A?}A �/A Z@���@��y@�@���@���@���@�5?@���@�ff@���@��m@�M�@��/@@�hs@��
@�ȴ@�J@�@���@�j@�F@�V@�@��@�  @���@�E�@��@��@܋D@ۅ@�;d@ڧ�@ٺ^@�1'@�S�@��@ԣ�@җ�@�X@�9X@�t�@�ȴ@��@̛�@˅@ʰ!@��#@��@�1@�|�@�K�@�^5@�O�@�%@Ĭ@�1'@�ƨ@��@�E�@���@�V@�Z@���@��@���@��/@�Z@�A�@��m@��R@���@���@�Ĝ@�  @�S�@��!@�J@�G�@�Ĝ@�I�@���@��@�S�@��y@�~�@�{@�%@��@��`@���@�1'@�l�@�;d@��@��@�M�@���@�x�@�/@��@��@�bN@�I�@�(�@��@�1@��w@���@�t�@�+@�ȴ@��+@�^5@���@�V@�7L@���@�"�@��y@���@���@���@��h@�&�@���@��j@��
@���@�t�@�S�@�+@��R@�v�@�=q@��T@��@�p�@�O�@���@���@�Z@��@���@�S�@�o@�@��@���@�@��h@�O�@��@��@�%@���@��`@���@���@��@���@�@�E�@��\@�v�@��/@��D@��`@��9@��@��@�Z@�(�@�  @��@���@��@�K�@��@�l�@���@�K�@�o@��@�ȴ@��R@�5?@��@�$�@�$�@�@��^@��h@�G�@��@���@��D@�Z@�Q�@�I�@�(�@�b@���@��m@��w@�l�@�"�@�
=@�@���@��y@��R@��!@�M�@��-@�x�@��@�V@���@��`@���@��@���@��D@���@�r�@�Q�@�(�@�S�@�
=@��@��H@���@�n�@�{@��#@�@��^@���@�`B@�%@�z�@��m@���@�S�@�33@��@���@�v�@��@���@���@��h@��7@�`B@�V@���@�Z@�9X@�b@��@�ƨ@��@���@���@���@���@�dZ@��@�ȴ@��+@�v�@�n�@�~�@�ff@�ff@�-@�-@��@��@�V@��@�Z@�Q�@�j@�Q�@��F@�+@�
=@�@��H@��@���@�V@�E�@�5?@�J@��#@��-@�hs@��`@���@�r�@�j@�j@�1@���@�\)@�"�@���@�v�@�v�@�^5@�5?@���@���@���@�hs@�O�@�V@��9@+@~��@~E�@}�@}?}@}�@|�/@|�D@|�@{t�@{@z��@z��@zn�@z�@y��@y�7@yhs@yG�@y7L@y7L@x�`@x1'@w�@w��@w;d@v�@v�R@vff@u@t�@s��@s�@st�@sdZ@s33@r�@r��@rn�@q�#@qX@pĜ@pA�@o�w@o|�@o;d@nȴ@n�+@n$�@m��@m�h@m`B@m�@l�j@l�@k�m@kS�@j�!@j~�@i��@h��@hĜ@h��@hQ�@h �@g�@g�w@g�P@g+@f��@fE�@e�@e��@e�-@e�h@eO�@e�@d�/@dz�@d1@c��@c��@cS�@c33@co@b�H@b=q@a��@ax�@a%@`��@`1'@_�@_�@^�@^��@^v�@^5?@]�@]��@]`B@]/@\�@\�D@\Z@\I�@\1@[��@[dZ@Z�H@Z��@Z��@Z��@Z~�@Z-@ZJ@Y�@Y�#@Y��@Y��@Y�7@Yx�@X��@XA�@X �@W�@W�w@W\)@W+@W+@W�@V��@V�@V��@VE�@V{@U��@U`B@T�/@Tz�@T(�@T1@S��@S�m@S33@R�\@R-@Q��@QX@Q%@P�u@Pb@O��@Ol�@N��@NV@M@MO�@M/@L�@L��@L�@L9X@Kƨ@KS�@K"�@K33@K@J��@JJ@I��@Ix�@I&�@H�`@H��@HbN@G+@F�+@F5?@F@E`B@E�@D��@Dj@D9X@C��@Cƨ@C��@Ct�@Co@B��@B�\@Bn�@BM�@B=q@B=q@B-@A��@@�9@@r�@@ �@@  @?��@?l�@?+@>��@=��@=`B@=�@<��@<��@<�@<�@<��@<Z@;dZ@;@:�H@:��@:��@:�\@:~�@:~�@:M�@:M�@:=q@:=q@:�@9�#@9�^@9x�@8bN@8b@7�@7�;@7�w@7K�@7K�@7�@6ȴ@6�+@6V@6$�@6{@5�T@5�h@5`B@5�@4��@4Z@4�@41@3��@3C�@333@3@2�!@2M�@2J@1��@1�#@1��@1X@0��@0��@0Q�@0b@0b@0  @/�w@/\)@/�@.�R@.�+@-�@-�@-�@-p�@,��@,��@,j@,I�@,(�@+�
@+"�@*�H@*��@*�\@*^5@*=q@*J@)�^@)�7@)X@)7L@)%@(Ĝ@(��@(Q�@(A�@(  @'�@'�P@'K�@'
=@&�y@&��@&$�@%�-@%�h@%p�@%p�@%O�@%/@$��@$��@$z�@$I�@$�@#�
@#��@#dZ@#S�@#S�@#33@#"�@"��@"��@"��@"~�@"=q@"-@!�@!��@!��@!�7@!hs@!7L@ �`@ 1'@   @�;@�@��@�P@|�@\)@K�@K�@;d@+@
=@�@��@�+@V@�T@�-@��@�h@O�@/@�@��@�j@�@�D@z�@j@z�@j@I�@1@�m@ƨ@��@��@��@t�@dZ@C�@�@��@��@��@M�@�@�@�#@��@��@��@�7@��@�@bN@ �@��@+@+@�@
=@�y@�+@V@@��@�-@�@p�@p�@O�@?}@V@�/@��@�j@��@I�@1@ƨ@��@33@�H@�!@�\@n�@M�@�#@��@hs@7L@&�@�@��@Ĝ@�9@��@�@  @�;@�w@\)@;d@+@�@�@�R@�R@��@v�@5?@��@�h@O�@V@��@Z@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�)B�#B�)B�#B�B��B��B�jBR�Bq�Bw�B{�B�=B�B}�B�Bt�B{�Bu�B�VB�\B�\B�=B� Bs�Bq�Bs�Br�Bs�BjBgmBaHBW
BO�BH�B?}B1'B'�B�B�B�B
=B�B�B�`B��BB�B�uB}�B|�B�B�DB�1B�Bq�BXBQ�BL�BXBN�BI�BG�BI�BF�B:^B33B&�BB
�mB
�
B
�qB
��B
{�B
e`B
s�B
dZB
O�B
A�B
C�B
:^B
$�B
{B
DB	��B	��B	�B	��B	�!B	��B	�hB	�%B	{�B	� B	u�B	ffB	iyB	dZB	aHB	\)B	R�B	H�B	I�B	<jB	,B	\B	VB	�B	{B	%B	PB		7B	B��B��B�B�B�`B��B��BƨB�FB�9B�B�!B�B��B��B�%B��B�\B�+Bv�Bu�Bu�Bq�B{�B�DB�+B�PB�bB�DB�B�B}�By�Bp�Bt�Bw�Bv�Bs�Bx�B}�B�B~�Bw�Bl�BgmBk�BiyB]/BW
B\)B[#B[#B_;B[#B\)BZBcTBdZB^5B^5BaHB`BB`BB_;B^5B_;B`BB_;B[#B^5BcTBbNB`BB]/B_;BZBZBYBYBZBT�BP�BJ�BP�BJ�BK�BL�BK�BG�BM�BS�BW
BVBW
BT�BT�BR�BW
BYBW
BW
BXBS�BL�B]/BaHBgmBe`BaHB_;BffBffB^5BcTBl�Bp�Bt�Bv�Bv�Bt�Bv�B}�B~�B�B�B�7B�JB�7B�=B�oB�uB�oB�{B�uB�{B��B��B��B��B��B��B��B�B�B�B��B�-B�LB�FB�dB�}BÖBƨBɺB��B��B�
B�B�#B�/B�BB�TB�TB�B�B�B�B��B��B	B	B	+B	VB	bB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	+B	.B	/B	2-B	6FB	8RB	7LB	7LB	A�B	:^B	<jB	E�B	?}B	G�B	G�B	H�B	I�B	N�B	K�B	K�B	P�B	R�B	T�B	T�B	T�B	YB	]/B	^5B	`BB	e`B	e`B	dZB	gmB	jB	m�B	o�B	u�B	w�B	z�B	z�B	z�B	y�B	� B	�B	�7B	�DB	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�?B	�RB	�jB	�dB	�jB	�jB	�jB	�qB	�jB	�wB	��B	B	B	B	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�
B	��B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�`B	�fB	�mB	�mB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
%B
+B
B
B
%B
+B
1B
	7B

=B

=B
+B
%B

=B
DB
DB
JB
PB
DB
VB
\B
\B
\B
\B
VB
PB
\B
bB
bB
bB
\B
PB
VB
VB
PB
bB
oB
hB
hB
hB
hB
hB
oB
{B
uB
uB
bB
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
(�B
'�B
(�B
)�B
)�B
(�B
,B
,B
,B
,B
-B
-B
-B
,B
,B
-B
-B
.B
.B
.B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
/B
.B
.B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
2-B
49B
49B
33B
33B
33B
33B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
5?B
49B
33B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
6FB
7LB
8RB
8RB
7LB
5?B
6FB
7LB
8RB
7LB
9XB
8RB
9XB
:^B
;dB
:^B
:^B
:^B
<jB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
>wB
>wB
>wB
>wB
=qB
?}B
?}B
?}B
?}B
?}B
>wB
<jB
>wB
?}B
?}B
@�B
B�B
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
F�B
F�B
F�B
F�B
E�B
D�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
H�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
M�B
M�B
L�B
J�B
M�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
R�B
R�B
T�B
S�B
S�B
S�B
T�B
VB
VB
T�B
T�B
T�B
VB
VB
W
B
XB
XB
W
B
W
B
W
B
W
B
XB
W
B
XB
ZB
ZB
YB
YB
ZB
ZB
ZB
ZB
YB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
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
e`B
dZB
dZB
dZB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
gmB
hsB
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
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
k�B
jB
l�B
m�B
l�B
l�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
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
r�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
x�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�)B�=B�CBۦBچB�\B�"B�BV�BtTBz*B~]B��B�?B�iB�Bw�BBx�B�VB�vB��B�xB��Bw�BshButBt�Bu�Bl�BiDBc BYBRoBJ�BA�B3�B*0B vB5B�B~B��B��B�RB�YB�9B��B��B��BcB�B��B�B��BtB[qBT{BN�BX�BPBJ�BH�BJ�BHB;�B4TB(�BEB
�=B
��B
��B
�B
�'B
h>B
u?B
f�B
R�B
C�B
D�B
;�B
'mB
$B
�B	�(B	�B	�oB	�NB	��B	��B	��B	��B	~]B	� B	w�B	h�B	jB	ezB	bB	]IB	T,B	JXB	J�B	>(B	.�B	@B	4B	�B	�B	1B	"B	
=B	gB�(B��B�!B�B��B�YB��B�lB��B�LB� B��B��B�HB�_B�B��B� B�lBy�Bw�BwfBs�B|�B��B�KB��B� B�0B�YB�BHB{Br�Bu�Bx�Bw�BuBy�B~wB��B�By�BoOBi�Bl�Bj�B_VBX�B]�B\xB\�B`�B\xB]�B[=Bc�BeB_pB_!Ba�BaBa-B`BB_pB`'B`�B_�B\CB^�Bc�Bb�B`�B^B_�B[	BZ�BY�BY�BZ�BU�BR BL0BQ�BLBMBM�BL�BI7BN�BT�BW�BV�BWsBU�BU�BS�BW�BY�BW�BW�BX�BUMBN�B^BbBg�Be�Bb4B`\Bg8BgB_�Bd�BmwBqvBuZBwfBw�Bu�Bw�B~�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�WB�5B�dB��B��B�kB�OB�wB��B��B��B��B�B� B�3B�+B�XB�HB�{B�YB�kB�qBݘB�B��B�B��B��B��B�3B�LB�.B	;B	gB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	($B	+6B	.IB	/iB	2|B	6�B	8�B	7�B	7�B	A�B	;dB	<�B	E�B	@4B	G�B	G�B	H�B	J#B	N�B	L~B	LdB	QB	S&B	UB	UMB	U�B	YKB	]dB	^�B	`�B	ezB	e�B	d�B	g�B	j�B	m�B	pB	vB	xB	z�B	z�B	{0B	zxB	�OB	�SB	�RB	�^B	�dB	�pB	�}B	��B	��B	�sB	�WB	��B	��B	��B	�,B	��B	�$B	��B	�]B	�5B	�UB	�[B	�aB	�GB	�GB	�aB	�aB	�aB	�ZB	�8B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	� B	�B	�B	�&B	�B	�
B	�$B	�?B	�9B	�?B	�gB	�mB	�WB	�]B	�OB	�VB	�\B	�\B	�bB	�hB	�nB	�`B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�>B	�B	�(B	�B
B
B
?B
EB
tB
_B
mB
�B
tB
zB
KB
	7B

=B

rB
�B
tB

XB
xB
^B
dB
jB
�B
pB
vB
vB
�B
�B
�B
�B
�B
}B
}B
}B
�B
�B
�B
�B
�B
}B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
�B
�B
�B
�B
WB
 �B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
"B
#B
#B
$B
%B
&B
%�B
&B
'B
'B
'B
(
B
(
B
(
B
($B
'B
)B
($B
)*B
*B
*0B
)DB
,"B
,"B
,"B
,"B
-)B
-)B
-)B
,=B
,=B
-)B
-)B
./B
./B
./B
-)B
-)B
-)B
-CB
./B
/5B
/5B
/5B
0;B
0UB
/5B
.cB
.IB
0;B
0oB
0UB
0UB
0oB
0UB
1AB
2aB
2GB
2GB
2aB
2GB
2GB
3MB
3MB
2aB
4TB
4TB
3MB
3hB
3MB
3hB
5ZB
5ZB
5ZB
5ZB
5ZB
5tB
5ZB
6`B
5ZB
6FB
6FB
5ZB
4nB
3�B
6`B
6`B
6`B
6`B
6zB
7LB
7LB
6`B
6`B
6`B
6`B
6zB
6`B
5tB
5tB
6`B
7fB
8lB
8lB
7fB
5�B
6zB
7�B
8lB
7�B
9rB
8�B
9�B
:�B
;B
:�B
:�B
:�B
<�B
<�B
<�B
<�B
<�B
;�B
<�B
<�B
>�B
>wB
>�B
>�B
=�B
?�B
?�B
?�B
?�B
?�B
>�B
<�B
>�B
?�B
?�B
@�B
B�B
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
F�B
F�B
F�B
F�B
E�B
D�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
IB
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
M�B
M�B
MB
KB
M�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
Q B
Q B
Q B
RB
RB
Q B
RB
RB
R:B
SB
SB
TB
S&B
SB
T�B
TB
TB
T,B
U2B
VB
VB
UB
UB
U2B
VB
VB
W?B
XB
X+B
W$B
W?B
W$B
WYB
XEB
W?B
X+B
ZB
ZQB
YKB
Y1B
Z7B
Z7B
Z7B
Z7B
YKB
[=B
\CB
\CB
\CB
]IB
]IB
]IB
]IB
]IB
^jB
^OB
^OB
^OB
^OB
_VB
^jB
_pB
_VB
_VB
_VB
`\B
_pB
_pB
`vB
abB
bNB
bNB
b�B
abB
abB
bhB
a|B
bhB
bhB
bhB
bhB
cnB
cTB
cTB
cTB
cnB
c�B
dZB
dtB
dtB
dtB
dtB
dtB
ezB
ezB
e�B
ezB
dtB
d�B
d�B
f�B
f�B
g�B
gmB
gmB
gmB
g�B
gmB
hsB
hsB
g�B
g�B
g�B
g�B
hsB
h�B
g�B
h�B
iyB
i�B
i�B
i�B
iyB
i�B
i�B
jB
j�B
jB
k�B
k�B
j�B
j�B
j�B
j�B
j�B
k�B
kkB
k�B
k�B
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
k�B
j�B
l�B
m�B
l�B
l�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
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
r�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
x�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333111111131111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111131111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<F?<I��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712010034292017120100342920171201003429201806221322342018062213223420180622132234201804050725352018040507253520180405072535  JA  ARFMdecpA19c                                                                20171127063512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171126213513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171126213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171126213516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171126213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171126213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171126213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171126213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171126213517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171126213517                      G�O�G�O�G�O�                JA  ARUP                                                                        20171126215532                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171127153444  CV  JULD            G�O�G�O�F�Ü                JM  ARSQJMQC2.0                                                                 20171129000000  CF  PSAL_ADJUSTED_QCCL  D�� G�O�                JM  ARCAJMQC2.0                                                                 20171130153429  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171130153429  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222535  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042234  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                