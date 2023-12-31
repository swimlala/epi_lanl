CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-03T00:35:23Z creation;2017-11-03T00:35:26Z conversion to V3.1;2019-12-19T07:53:31Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171103003523  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_176                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�2v�s 1   @�2v�l @4�6���d��@��41   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���C��C��C{C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D0D0�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du�RDv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܼ)D��\D�?\D�\Dݼ)D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��)D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A���A���AضFAخAجAة�Aإ�Aء�A�r�A�hsA�ffA�^5A�O�A�K�A�Q�A�^5A�ZA�ZA�M�A�9XA�(�A�&�A�/A��A��A׋DA���A�;dA�XA��A�1'A�{A͑hAˡ�A��A�ĜA��#A��A��TA� �A��TA¸RA���A�ȴA�XA�I�A�G�A�A��wA��FA��A��!A��7A��A�dZA�oA��uA�33A��HA��A��#A�S�A�A�l�A��#A�O�A�%A��;A���A��jA���A�VA�\)A�+A���A��^A�  A��A��A�JA��+A��`A��A���A�33A��!A�p�A���A���A��wA�+A�1A��FA�oA�
=A�G�A���A�C�A��yA��A��;A�|�A���A�-A���A~JA|�!A{��AxĜAu��As
=An�!Ak33Ah9XAe��Ae\)Ae%Ab��A`�A`(�A\�9AZ�AYl�AX��AX1'AUp�AT-ARffAQ��AQ�AQ�AO�ANZAKK�AJ �AI��AH��AG�PAF�jAE��ADVAC��AC�AB�ABJA@��A??}A>$�A<�+A;A:9XA9�A97LA8�HA6^5A5��A5oA3�A3%A2�A0�HA/��A/G�A.jA-�TA,�HA*�RA)x�A(VA'\)A'
=A&r�A$��A"��A!�TA!+A z�A�7A��A�hA?}Av�AC�A�A
=A��A�+A��A�HAAC�AĜAZA�A7LA�A�-A&�AjA��A�A
^5A	/A��AĜAbNA(�A��A\)AjA�A��A|�A\)A�yA��A{A�PA z�@�"�@��H@���@���@�1'@��;@���@�9@�E�@��@�ȴ@�I�@��@��@��@�bN@��
@�l�@�7L@�  @�
=@�{@�`B@�A�@ۍP@�M�@�z�@���@���@��@� �@�S�@ҧ�@�E�@�Ĝ@��@͑h@�I�@˶F@��@ʸR@��T@�?}@Ȭ@��@�v�@�n�@�V@���@��@�b@Õ�@�dZ@���@+@���@��h@��@�dZ@��@��\@��@�I�@��@�+@�ff@���@�p�@�?}@��D@�"�@��R@�@�%@�I�@��
@��!@�{@��@���@��@���@�I�@���@�ƨ@�|�@��@�~�@���@�p�@�X@�O�@�G�@�/@��@���@� �@��;@���@�
=@�ȴ@�^5@��@��#@���@��7@�&�@��@�  @�  @�  @��w@�
=@��H@�~�@�V@��@��h@�O�@�?}@��@���@��D@�I�@�1'@��@��w@�C�@���@���@�=q@�@��T@���@�@��-@��h@�G�@��`@�Ĝ@��u@�j@�Z@��m@��@��F@���@�(�@�bN@�Q�@�Q�@�z�@���@�b@��
@���@� �@��@��m@��;@���@�ƨ@��F@�S�@��@��R@���@��\@�M�@��@���@�hs@�O�@�&�@���@���@��;@���@���@��@�dZ@�C�@�+@�@��@�=q@��@���@�p�@�?}@��@���@���@��@�z�@�(�@��@��;@�ƨ@�|�@��@��@���@�v�@�M�@�{@��T@�p�@�/@���@��u@�Z@�9X@�1@��P@�|�@�|�@�|�@��@�|�@�;d@�v�@�M�@�5?@�$�@��@��@�V@�Ĝ@��9@��9@�j@���@��
@���@�S�@�
=@��y@��@���@���@���@��+@�~�@�E�@�J@���@��-@��7@�`B@�V@��`@��j@�r�@�r�@�r�@�(�@���@��m@��@�|�@�33@��R@��\@�ff@�E�@�-@�{@���@��T@�@��7@�O�@��@���@��j@��D@�r�@�Z@� �@���@�33@���@���@���@���@�G�@��@���@�9X@��
@���@�K�@�+@���@�$�@��@��^@��@��j@��@�I�@�9X@�1'@�@+@
=@�P@�1@�1@��@|�@~��@~ff@~�+@~v�@~E�@}�h@}�@|�@}V@}O�@|(�@z^5@z��@{33@{o@z�@z��@z-@y�7@yX@y�@x�9@x �@w|�@w+@w�P@w�P@w�P@w|�@v��@v5?@u�-@u�@up�@u`B@u/@t�@s��@r=q@q��@q��@q��@qG�@p�9@pQ�@p1'@p �@p �@pb@p  @o�@oK�@nff@m��@m��@m?}@m/@mV@l�@l�@lI�@l�@k��@k33@ko@j�@j��@j=q@jJ@i�@ihs@iG�@h��@g�@h  @h �@g��@gl�@g+@f�y@f��@f�+@f@e�@dz�@c�F@cƨ@c�
@c��@c�
@c��@c33@co@b�H@b��@b��@b��@bn�@a�#@a�7@a&�@a%@`��@`��@`��@`bN@`1'@_�;@_�P@^��@^V@^5?@]�T@]?}@\j@\(�@\(�@\(�@[S�@[@[@[33@[dZ@[C�@Z��@Y��@X��@X�@X1'@W�@W��@W�@W|�@W
=@V�+@V5?@U�T@UO�@T��@T��@T�@T��@UV@UV@T��@T��@TZ@Sƨ@SS�@R��@R��@R��@Rn�@Q�#@Q%@P��@P�@P1'@O�@O�P@O+@N�+@N$�@N$�@M�T@L��@L�@L�@L��@Lz�@LZ@K�
@Kt�@K"�@J�@J�!@J��@Jn�@I�#@IX@H�`@H��@HQ�@Hb@GK�@F�@Fv�@FV@FE�@F5?@F@E�T@Ep�@D��@D�D@C�F@B�H@BM�@A�#@A�^@Ax�@A7L@@��@@r�@@ �@?��@?��@?|�@?K�@?+@>�@>��@>ff@>ff@>V@>V@>E�@>@=�h@=`B@<�@<��@<Z@<(�@<1@;�m@;dZ@;C�@;dZ@;t�@;�@;dZ@;"�@;o@:n�@:�@:�@:�@:J@9��@9��@9��@9��@9X@97L@9&�@9%@9%@8�`@8�`@8�9@81'@7�w@7K�@7;d@7
=@6�R@6E�@5��@5�-@5�-@5��@5`B@5O�@5?}@5/@5/@5V@4j@3�m@3�@2~�@2J@1��@1��@1��@1��@1��@1X@1&�@0��@0�9@0Q�@0 �@/�@/��@/K�@.��@.V@-�@-�h@-O�@-V@,��@,j@,I�@,I�@,9X@,(�@+��@+�
@+��@+33@*�H@*�\@*M�@*=q@*J@)�^@)X@)%@(�`@(�9@(r�@(bN@(A�@(1'@( �@(b@(  @(  @'��@'��@'K�@&�@&��@&��@&�+@&V@&V@&5?@&5?@&{@&@%@%V@$�/@$��@$�D@$j@$Z@$9X@$�@$1@#�m@#�
@#�@#S�@#33@#@"�@"�@"�@"��@"�\@"^5@"M�@"M�@"=q@"-@!��@!��@!X@ �`@ r�@ Q�@ A�@ 1'@ b@�@��@��@�P@l�@\)@;d@+@�@
=@��@�@�R@ff@�@��@��@�h@�@p�@`B@/@�/@�@��@��@�D@z�@j@I�@�@��@33@o@@~�@^5@�@J@��@�@�#@hs@��@�u@bN@Q�@ �@�@��@��@K�@+@
=@
=@
=@��@ȴ@��@��@ff@5?@@�T@@��@�h@`B@/@�@V@�@�/@�D@�@�F@��@t�@33@33@33@@�!@�\@^5@=q@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A���A���AضFAخAجAة�Aإ�Aء�A�r�A�hsA�ffA�^5A�O�A�K�A�Q�A�^5A�ZA�ZA�M�A�9XA�(�A�&�A�/A��A��A׋DA���A�;dA�XA��A�1'A�{A͑hAˡ�A��A�ĜA��#A��A��TA� �A��TA¸RA���A�ȴA�XA�I�A�G�A�A��wA��FA��A��!A��7A��A�dZA�oA��uA�33A��HA��A��#A�S�A�A�l�A��#A�O�A�%A��;A���A��jA���A�VA�\)A�+A���A��^A�  A��A��A�JA��+A��`A��A���A�33A��!A�p�A���A���A��wA�+A�1A��FA�oA�
=A�G�A���A�C�A��yA��A��;A�|�A���A�-A���A~JA|�!A{��AxĜAu��As
=An�!Ak33Ah9XAe��Ae\)Ae%Ab��A`�A`(�A\�9AZ�AYl�AX��AX1'AUp�AT-ARffAQ��AQ�AQ�AO�ANZAKK�AJ �AI��AH��AG�PAF�jAE��ADVAC��AC�AB�ABJA@��A??}A>$�A<�+A;A:9XA9�A97LA8�HA6^5A5��A5oA3�A3%A2�A0�HA/��A/G�A.jA-�TA,�HA*�RA)x�A(VA'\)A'
=A&r�A$��A"��A!�TA!+A z�A�7A��A�hA?}Av�AC�A�A
=A��A�+A��A�HAAC�AĜAZA�A7LA�A�-A&�AjA��A�A
^5A	/A��AĜAbNA(�A��A\)AjA�A��A|�A\)A�yA��A{A�PA z�@�"�@��H@���@���@�1'@��;@���@�9@�E�@��@�ȴ@�I�@��@��@��@�bN@��
@�l�@�7L@�  @�
=@�{@�`B@�A�@ۍP@�M�@�z�@���@���@��@� �@�S�@ҧ�@�E�@�Ĝ@��@͑h@�I�@˶F@��@ʸR@��T@�?}@Ȭ@��@�v�@�n�@�V@���@��@�b@Õ�@�dZ@���@+@���@��h@��@�dZ@��@��\@��@�I�@��@�+@�ff@���@�p�@�?}@��D@�"�@��R@�@�%@�I�@��
@��!@�{@��@���@��@���@�I�@���@�ƨ@�|�@��@�~�@���@�p�@�X@�O�@�G�@�/@��@���@� �@��;@���@�
=@�ȴ@�^5@��@��#@���@��7@�&�@��@�  @�  @�  @��w@�
=@��H@�~�@�V@��@��h@�O�@�?}@��@���@��D@�I�@�1'@��@��w@�C�@���@���@�=q@�@��T@���@�@��-@��h@�G�@��`@�Ĝ@��u@�j@�Z@��m@��@��F@���@�(�@�bN@�Q�@�Q�@�z�@���@�b@��
@���@� �@��@��m@��;@���@�ƨ@��F@�S�@��@��R@���@��\@�M�@��@���@�hs@�O�@�&�@���@���@��;@���@���@��@�dZ@�C�@�+@�@��@�=q@��@���@�p�@�?}@��@���@���@��@�z�@�(�@��@��;@�ƨ@�|�@��@��@���@�v�@�M�@�{@��T@�p�@�/@���@��u@�Z@�9X@�1@��P@�|�@�|�@�|�@��@�|�@�;d@�v�@�M�@�5?@�$�@��@��@�V@�Ĝ@��9@��9@�j@���@��
@���@�S�@�
=@��y@��@���@���@���@��+@�~�@�E�@�J@���@��-@��7@�`B@�V@��`@��j@�r�@�r�@�r�@�(�@���@��m@��@�|�@�33@��R@��\@�ff@�E�@�-@�{@���@��T@�@��7@�O�@��@���@��j@��D@�r�@�Z@� �@���@�33@���@���@���@���@�G�@��@���@�9X@��
@���@�K�@�+@���@�$�@��@��^@��@��j@��@�I�@�9X@�1'@�@+@
=@�P@�1@�1@��@|�@~��@~ff@~�+@~v�@~E�@}�h@}�@|�@}V@}O�@|(�@z^5@z��@{33@{o@z�@z��@z-@y�7@yX@y�@x�9@x �@w|�@w+@w�P@w�P@w�P@w|�@v��@v5?@u�-@u�@up�@u`B@u/@t�@s��@r=q@q��@q��@q��@qG�@p�9@pQ�@p1'@p �@p �@pb@p  @o�@oK�@nff@m��@m��@m?}@m/@mV@l�@l�@lI�@l�@k��@k33@ko@j�@j��@j=q@jJ@i�@ihs@iG�@h��@g�@h  @h �@g��@gl�@g+@f�y@f��@f�+@f@e�@dz�@c�F@cƨ@c�
@c��@c�
@c��@c33@co@b�H@b��@b��@b��@bn�@a�#@a�7@a&�@a%@`��@`��@`��@`bN@`1'@_�;@_�P@^��@^V@^5?@]�T@]?}@\j@\(�@\(�@\(�@[S�@[@[@[33@[dZ@[C�@Z��@Y��@X��@X�@X1'@W�@W��@W�@W|�@W
=@V�+@V5?@U�T@UO�@T��@T��@T�@T��@UV@UV@T��@T��@TZ@Sƨ@SS�@R��@R��@R��@Rn�@Q�#@Q%@P��@P�@P1'@O�@O�P@O+@N�+@N$�@N$�@M�T@L��@L�@L�@L��@Lz�@LZ@K�
@Kt�@K"�@J�@J�!@J��@Jn�@I�#@IX@H�`@H��@HQ�@Hb@GK�@F�@Fv�@FV@FE�@F5?@F@E�T@Ep�@D��@D�D@C�F@B�H@BM�@A�#@A�^@Ax�@A7L@@��@@r�@@ �@?��@?��@?|�@?K�@?+@>�@>��@>ff@>ff@>V@>V@>E�@>@=�h@=`B@<�@<��@<Z@<(�@<1@;�m@;dZ@;C�@;dZ@;t�@;�@;dZ@;"�@;o@:n�@:�@:�@:�@:J@9��@9��@9��@9��@9X@97L@9&�@9%@9%@8�`@8�`@8�9@81'@7�w@7K�@7;d@7
=@6�R@6E�@5��@5�-@5�-@5��@5`B@5O�@5?}@5/@5/@5V@4j@3�m@3�@2~�@2J@1��@1��@1��@1��@1��@1X@1&�@0��@0�9@0Q�@0 �@/�@/��@/K�@.��@.V@-�@-�h@-O�@-V@,��@,j@,I�@,I�@,9X@,(�@+��@+�
@+��@+33@*�H@*�\@*M�@*=q@*J@)�^@)X@)%@(�`@(�9@(r�@(bN@(A�@(1'@( �@(b@(  @(  @'��@'��@'K�@&�@&��@&��@&�+@&V@&V@&5?@&5?@&{@&@%@%V@$�/@$��@$�D@$j@$Z@$9X@$�@$1@#�m@#�
@#�@#S�@#33@#@"�@"�@"�@"��@"�\@"^5@"M�@"M�@"=q@"-@!��@!��@!X@ �`@ r�@ Q�@ A�@ 1'@ b@�@��@��@�P@l�@\)@;d@+@�@
=@��@�@�R@ff@�@��@��@�h@�@p�@`B@/@�/@�@��@��@�D@z�@j@I�@�@��@33@o@@~�@^5@�@J@��@�@�#@hs@��@�u@bN@Q�@ �@�@��@��@K�@+@
=@
=@
=@��@ȴ@��@��@ff@5?@@�T@@��@�h@`B@/@�@V@�@�/@�D@�@�F@��@t�@33@33@33@@�!@�\@^5@=q@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BƨBƨBƨBǮBȴBȴBǮBǮBǮB��B��B�B�
B�B�B�;B�NB�`B�mB�fB�`B�ZB�fB�fB�HB�B��B��B�`B�NB�B�sB��B�B@�BYBK�BVBZB_;B]/BZBr�Bt�Bv�B�B�PB�+Bu�B�B�Bz�Bl�Be`BhsBjBz�B}�Bx�Bp�BffB]/BhsB[#B`BB`BBaHBcTBbNB]/BR�B49B�B49B)�B�BoBB�B�TB�HBÖB��B��B�hBiyB`BB`BBL�B:^B0!B�B
�B
��B
�HB
��B
��B
�JB
�VB
~�B
w�B
w�B
n�B
\)B
D�B
1'B
0!B
,B
hB	��B	�B	ƨB	�?B	��B	��B	��B	��B	�7B	y�B	{�B	]/B	W
B	YB	W
B	N�B	9XB	/B	+B	.B	.B	%�B	hB��B�B��B��B��B�B�B�fB�5B�ZB�;B�/B��BɺB��B��BÖB�}BĜBBB�dB��B�B�-B��B��B��B��B��B��B�uB�oB�7Bw�B�B�B�B�PB�Bz�Bu�Bz�B}�By�Bu�Bv�Br�Bx�Bo�BgmB`BB`BBZBW
B`BB]/B^5BdZBaHBaHB`BBYBW
BP�BW
BQ�BF�BF�B>wBD�B@�BJ�BQ�BS�BQ�BK�BI�BQ�BS�BQ�BS�BO�BO�BM�BK�BD�BF�B>wBE�BG�BO�BW
BQ�BJ�BG�BN�BK�BH�BS�BZB]/B]/B\)BZBP�BXB\)B^5B_;B^5B`BB^5B[#BgmBffBgmBdZBo�Bq�Bq�Bk�Bk�Bt�By�B�B�B�%B�B�+B�1B�B�PB�uB�oB�bB�\B�\B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�9B�RB�^B�RB�RBŢBƨBȴB��B�B�B�BB�mB�sB�sB�B�B�B�B�B�B��B��B	%B		7B	DB	DB	
=B	
=B	JB	\B	uB	�B	�B	�B	�B	�B	$�B	%�B	$�B	$�B	&�B	-B	33B	33B	1'B	0!B	7LB	7LB	;dB	<jB	?}B	B�B	E�B	F�B	G�B	I�B	K�B	N�B	O�B	P�B	Q�B	VB	XB	ZB	^5B	aHB	bNB	dZB	dZB	e`B	ffB	gmB	jB	k�B	n�B	o�B	r�B	w�B	{�B	� B	�B	�=B	�DB	�JB	�\B	�VB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�9B	�9B	�9B	�9B	�^B	�wB	�}B	��B	��B	��B	��B	��B	�}B	��B	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�/B	�BB	�;B	�;B	�ZB	�`B	�ZB	�ZB	�TB	�HB	�;B	�ZB	�`B	�ZB	�TB	�NB	�ZB	�mB	�yB	�yB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
+B
+B
B
1B

=B
	7B

=B
1B
	7B

=B

=B
1B
DB
\B
PB
bB
hB
hB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
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
�B
�B
 �B
 �B
 �B
 �B
"�B
$�B
$�B
$�B
$�B
#�B
#�B
!�B
 �B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
$�B
$�B
#�B
"�B
#�B
"�B
"�B
#�B
"�B
"�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
%�B
$�B
$�B
$�B
)�B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
,B
+B
+B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
,B
-B
.B
/B
0!B
/B
/B
/B
2-B
33B
2-B
0!B
2-B
33B
49B
49B
2-B
0!B
/B
.B
/B
/B
0!B
0!B
0!B
/B
/B
/B
0!B
1'B
0!B
2-B
49B
49B
49B
6FB
6FB
6FB
6FB
5?B
49B
5?B
49B
6FB
7LB
7LB
6FB
7LB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
;dB
8RB
;dB
<jB
<jB
<jB
;dB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
;dB
;dB
<jB
=qB
=qB
=qB
<jB
>wB
@�B
B�B
B�B
A�B
A�B
@�B
?}B
?}B
>wB
>wB
?}B
A�B
B�B
D�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
I�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
N�B
N�B
N�B
P�B
O�B
O�B
O�B
P�B
Q�B
R�B
Q�B
R�B
R�B
S�B
S�B
R�B
Q�B
P�B
P�B
Q�B
P�B
S�B
T�B
VB
VB
VB
VB
VB
VB
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
VB
T�B
W
B
W
B
XB
XB
XB
XB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
YB
YB
YB
ZB
ZB
[#B
[#B
ZB
[#B
[#B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
`BB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
`BB
_;B
^5B
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
bNB
cTB
cTB
cTB
cTB
e`B
e`B
ffB
e`B
e`B
ffB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
iyB
iyB
hsB
hsB
iyB
jB
k�B
jB
k�B
k�B
l�B
l�B
l�B
l�B
jB
k�B
l�B
m�B
n�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BƨB��B��B��BȴBȴB��B��B��B��B��B�B�$B�B�B�!B�NB�`B�B�B�zB�ZB�fB�B��B�?B��B��B�8B�,B��B��B�eB#BCGBZ�BO�BXB\]Ba-B_�B\�Bt�BwLBy�B��B��B��By�B�B�{B|PBo�Bh�Bk�Bm)B{�B~�By�Br�BiDB`BBjB]�Ba|BabBa�Bc�Bb�B]�BT{B9	B �B4�B+kB�BaB�B��B��B�TB��B��B�+B��Bn�BcBa�BO(B<�B1�B�B
�tB
��B
��B
ΊB
�6B
��B
�NB
�oB
z*B
x�B
p;B
^�B
HfB
4�B
1�B
-�B
MB	��B	�!B	��B	�rB	�eB	�NB	��B	��B	�B	|6B	}qB	`�B	YeB	Z�B	XB	PHB	<jB	0�B	,�B	.�B	.cB	&�B	�B�wB��B�2B��B�B�B��B�B�B�B�'B�B�BˬBбB�pBżB�UBŢB�{B�-B�jB��B�)B�3B�yB�,B��B��B��B��B��B�uB��Bz�B��B��B�YB��B�MB}Bx8B|PB~�Bz�BwBxBtByrBp�Bi*Ba�Ba�B[�BX�BaHB^�B_�BeBbBa�B`�BZQBW�BR:BXBS@BH�BHKB@�BFYBB�BK�BRoBTaBRoBL�BJ�BR�BT{BRoBTFBP�BP}BN�BL�BFYBHfBA BGEBI�BP�BW�BR�BLJBIRBO�BMjBJ�BT�BZ�B]�B]�B\�BZ�BRTBX�B\�B^�B_�B_B`�B_;B\]Bh
Bg8Bh>Be�Bp;BrGBr-Bl�Bl�Bu�Bz�B�{B��B��B��B��B��B�%B��B��B��B��B��B�B��B��B��B��B�7B�#B��B�xB�:B�FB��B��B��B��B��B��B��B��B�	B�rB��B�EB�lB�vB�mB��B�B�B��B�B��B��B�B��B��B�B�?B�rB	?B		lB	^B	^B	
rB	
�B	�B	�B	�B	�B	�B	�B	�B	B	$�B	%�B	%,B	%FB	'mB	-]B	3MB	3MB	1vB	0�B	7�B	7�B	;�B	<�B	?�B	B�B	E�B	F�B	HB	I�B	K�B	N�B	PB	Q4B	RTB	V9B	XyB	Z�B	^jB	abB	b�B	dtB	dtB	e�B	f�B	g�B	j�B	k�B	n�B	o�B	r�B	xB	|B	�B	�B	�#B	�^B	�JB	�BB	�pB	��B	��B	�sB	��B	��B	��B	��B	��B	��B	��B	� B	�2B	�0B	�)B	�"B	�=B	�IB	�}B	�aB	�TB	�nB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	� B	�4B	�&B	�2B	�B	�?B	�?B	�EB	�_B	�WB	�]B	�xB	�dB	�\B	�pB	ߤB	�ZB	�`B	�ZB	�ZB	�nB	�B	߾B	�B	�zB	�tB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�	B	��B	��B	�B	�B	�B	�B	�6B	�<B	�<B	�.B	�B
 B
 B
 B
 B
 B
 B
 4B
;B
;B
AB
3B
3B
MB
MB
AB
[B
�B
oB
uB
?B
zB
tB
_B
zB
�B
�B

rB
	�B

rB
�B
	�B

rB

�B
�B
�B
vB
�B
�B
�B
�B
�B
uB
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
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
 �B
�B
!B
�B
 �B
 �B
 �B
�B
B
B
#B
�B
 �B
 �B
 �B
 �B
"�B
$�B
$�B
$�B
$�B
#�B
#�B
"B
!-B
#B
$B
$�B
%�B
&B
%�B
%�B
%B
$�B
$B
$&B
%B
$�B
#�B
#B
$B
"�B
#B
#�B
# B
# B
%�B
&�B
&B
'B
'B
'B
'B
(
B
&B
%,B
%,B
%,B
)�B
)�B
)�B
)B
)B
)B
*B
*B
+B
,"B
+B
+B
*0B
*B
+B
,"B
,=B
,"B
,"B
,"B
-)B
,=B
-CB
.}B
/5B
0;B
/OB
/iB
/iB
2GB
33B
2GB
0oB
2aB
33B
49B
49B
2GB
0oB
/iB
.cB
/OB
/OB
0;B
0;B
0;B
/OB
/iB
/OB
0;B
1AB
0UB
2GB
49B
49B
49B
6FB
6`B
6`B
6`B
5ZB
4nB
5tB
4nB
6`B
7fB
7�B
6�B
7�B
9rB
:xB
:xB
:xB
:�B
:�B
:�B
;B
<jB
;�B
8�B
;dB
<jB
<�B
<�B
;B
:�B
:�B
;B
;B
;B
<�B
<�B
;�B
;�B
<�B
=�B
=�B
=�B
<�B
>�B
@�B
B�B
B�B
A�B
A�B
@�B
?�B
?�B
>�B
>�B
?�B
A�B
B�B
D�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
I�B
K�B
M�B
M�B
M�B
M�B
NB
M�B
MB
M�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q B
P�B
Q B
P�B
O�B
OB
OB
OB
P�B
O�B
O�B
PB
Q B
RB
R�B
RB
SB
R�B
S�B
S�B
R�B
R B
QB
QB
R B
QhB
TB
UB
VB
VB
VB
VB
VB
VB
VB
W$B
VB
W$B
W$B
W$B
V9B
U2B
W?B
W?B
X+B
X+B
X+B
XEB
ZQB
ZQB
[=B
ZQB
Z7B
Z7B
Z7B
Y1B
Y1B
Y1B
Z7B
Z7B
[#B
[=B
ZkB
[=B
[=B
]IB
]dB
^OB
_;B
_VB
_;B
_;B
_;B
_;B
_;B
^OB
^OB
^OB
^�B
`\B
aHB
`\B
`\B
aHB
abB
aHB
abB
`\B
_pB
^�B
abB
b�B
bNB
bhB
cTB
c�B
cnB
cTB
cnB
cnB
bhB
cnB
dZB
cnB
dZB
dZB
dZB
dtB
cnB
dtB
dZB
dtB
dZB
cnB
b�B
cnB
cnB
c�B
c�B
ezB
ezB
ffB
ezB
e�B
f�B
gmB
ffB
ffB
f�B
gmB
gmB
gmB
gmB
gmB
g�B
f�B
f�B
f�B
hsB
g�B
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
jB
jB
j�B
i�B
i�B
h�B
h�B
i�B
j�B
k�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
j�B
k�B
l�B
m�B
n�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711070036212017110700362120171107003621201806221321272018062213212720180622132127201804050724182018040507241820180405072418  JA  ARFMdecpA19c                                                                20171103093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171103003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171103003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171103003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171103003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171103003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171103003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171103003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171103003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171103003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20171103005458                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171103153707  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171106153621  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171106153621  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222418  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042127  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                