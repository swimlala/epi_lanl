CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-22T00:35:24Z creation;2016-08-22T00:35:27Z conversion to V3.1;2019-12-19T08:28:41Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160822003524  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_030                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�����1   @��� <��@49�����d��d��81   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>�R@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D�RD~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3�RD4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D\D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�|)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AᛦA��A��A��A�A�A�A�A�-A�-A�!A�A�A�!A�!A�-A�-A�-A�-A�9A�9A�9A�9A�FA�FA�FA�RA�RA�^A�^A�RA�FA�7LA�l�A���AۃA�-A���A�ffA�1A�dZA�JA�7LA�  AѸRA�9XA�5?A�p�A͙�A�^5A�5?A�{AǛ�AƃA�\)A�\)A©�A�  A�A��yA��A�I�A�ƨA�ffA�G�A�^5A��jA��A�x�A�G�A���A�VA��A� �A�^5A�A�A�p�A���A�A�VA��wA��+A��A��hA���A�VA�{A��HA�9XA��DA��TA�1A��
A�~�A��mA�{A���A���A�l�A�`BA���A�\)A�A�bA���A���A�v�A�7LA���A�&�A�1A��!A�XA�p�AƨAS�A|jAx��AwO�AvffAu"�As�ArE�AqoAnv�Am��AmhsAl-Aj��Ai�#Ag�Ac7LAa?}A^�`A]��A\��A\VAY�AX�HAW��AU�
AS33AR�AQS�AO�TAN5?AI|�AHffAG��AG�PAG�AE�
AE&�AC�AB�DAA�A@bNA@A�A@9XA?�#A?33A<��A:~�A:(�A9x�A8^5A8(�A7ƨA7VA5�7A2-A0�\A/�^A/�A-|�A,VA,JA+��A*��A*~�A*(�A(�`A&��A%�A$�+A#7LA ȴA��AbA�PA`BA�A�Av�AA�jA��A��A��A�A
=AȴAv�A��AO�A�A1'A-A33A��A��AĜA��AffA
n�A%A�AƨA&�A�A��A�AA�A�A �@�;dA �D@��@�j@�&�@�%@�%@�?}@��+@�hs@�w@�hs@��-@�+@�=q@�p�@�&�@�bN@�(�@��@���@�dZ@��@�E�@�V@�o@���@ڗ�@ؼj@�  @��@�S�@��@�|�@�t�@�dZ@�x�@�p�@���@؋D@�`B@ڇ+@��@�V@�+@�|�@���@���@և+@�G�@��m@�5?@�O�@Гu@��m@���@�{@�hs@˥�@�ȴ@�^5@�J@��@Ǿw@���@��T@�r�@�1@���@��m@��@�ff@�E�@�@���@���@�X@��@��@�"�@�^5@��7@�%@�t�@�^5@�5?@���@�o@�C�@���@�^5@��#@�p�@�p�@�G�@��@�&�@�/@�hs@�z�@�dZ@�ȴ@�dZ@���@��m@��w@���@�ff@��@��-@���@���@�G�@�V@��@�7L@�7L@�/@��@���@�b@��
@���@��@���@��P@��@�@���@���@�V@�%@�&�@�/@�A�@��m@�l�@�@�ȴ@��@���@�V@��h@��@���@�?}@���@��u@�z�@�(�@���@���@��w@���@���@�  @��m@���@���@�x�@�V@��m@��P@�t�@��@�n�@�$�@�$�@��@�/@��/@��9@���@�Z@�I�@�9X@��@��F@���@�"�@�=q@��7@�%@��j@�z�@��@��@�v�@�$�@��@�@��7@�`B@�&�@��@�Z@�b@���@��w@�ƨ@���@��w@�C�@��@��@��+@��^@��@�J@��@�@��7@�x�@�x�@�X@���@�%@��@��`@��D@�z�@�j@�Q�@�I�@�A�@�9X@�(�@�1'@��@��m@��F@���@��@�S�@�+@�
=@���@�^5@�E�@�=q@��@��#@���@�O�@��@�%@��@���@��@� �@��@�l�@�C�@�o@���@���@�~�@�5?@�@��#@��#@���@�@��7@��@�G�@���@��9@�Q�@�I�@�1@�ƨ@���@��@�l�@�S�@�33@��y@���@��\@�~�@�^5@��@�p�@�&�@���@���@���@�r�@� �@���@�l�@�C�@���@�n�@�-@�5?@���@�G�@���@��@�
=@��@��y@��!@�~�@�-@��@���@�X@��@���@��`@���@�1@�ƨ@��P@�\)@�S�@�;d@�
=@�ȴ@�n�@�5?@�@�@�`B@�?}@��@��j@��9@��9@���@��@�bN@�b@�b@�@�@K�@�@~�@~��@~ff@~V@~5?@}�T@}@}�@}/@}V@|�/@|(�@{�F@{��@{�@{C�@{o@z�!@zn�@z�@y�#@yx�@y�@xĜ@x�@x  @w�P@vȴ@u�@t�@tZ@t1@s�@r��@q��@qhs@q&�@p�@o�;@ol�@n�+@n@m�-@m�h@m?}@mV@l�@l�@l�/@l��@l��@l�@l�D@l9X@k�
@k��@kC�@j��@jM�@i��@i��@ix�@h��@h��@hr�@hA�@g��@g
=@fȴ@f�+@f5?@e�@d�/@d�j@d��@dj@dI�@c�
@cS�@c"�@co@c@b��@b~�@a�7@`�u@`  @_|�@_�@^�@^v�@^V@^{@]��@]`B@\��@\�D@\9X@[�@Z�\@Y��@Y�7@YX@XĜ@XQ�@W�P@W+@V��@Vȴ@V��@V$�@U��@U�h@T�/@TI�@T�@S�m@S��@S33@So@R��@Q�@P��@P�9@P�u@PbN@P1'@O�@Ol�@N��@Nff@N{@M�T@M@M��@M�h@Mp�@L��@L��@L�@Lz�@L(�@KC�@J��@J�!@J�!@J~�@JM�@J�@I��@I�7@H��@Hr�@HA�@H �@G�P@G+@F��@F�@F�@Fȴ@F��@E`B@C�m@CdZ@C"�@B�@B��@B��@B��@B�!@B��@B�\@BM�@BM�@B=q@A��@AX@@��@@��@@ �@?��@?l�@?�@>��@>ȴ@>��@>ff@>V@>V@>E�@>$�@=��@=?}@<��@<z�@<(�@<1@;�
@;ƨ@;�F@;��@;S�@;@:��@:��@:��@:�!@:~�@:~�@:~�@:~�@:^5@9��@9��@9X@9X@9X@9G�@9&�@8�9@8�@8r�@8bN@8A�@8  @7��@7|�@7\)@7�@6ȴ@6�R@6�R@6��@6ff@6E�@6{@5�T@4�/@4z�@4I�@3�
@3�@333@3o@2�H@2�\@2�@1��@1G�@17L@1&�@0�`@0�u@01'@/�;@/��@/l�@/+@/
=@.ȴ@.V@-��@-�@-?}@,�@,�@,Z@,�@+��@+C�@+@*M�@)�#@)x�@)�@(�`@(Ĝ@(bN@'��@'��@'|�@'K�@'+@&�@%�@%@%@%�-@%�h@$�@$��@$�/@$�/@$�/@$�@$�/@$�@$Z@#ƨ@#dZ@#o@"�\@"M�@"=q@"�@"J@!��@!�@!�^@ ��@ �u@ bN@ Q�@ A�@  �@�@�P@
=@�+@$�@�@��@�h@�h@�h@�@p�@/@�@��@�@�@�@��@�j@�@��@z�@(�@��@�m@ƨ@S�@33@o@@@@��@�\@M�@��@��@��@x�@%@Ĝ@�9@�@A�@�;@�w@�P@�P@|�@l�@\)@K�@;d@+@+@��@ȴ@��@V@�T@��@�@O�@/@�@��@�j@��@�D@j@I�@1@�
@ƨ@t�@��@��@��@�\@n�@M�@J@�@�^@hs@7L@�@%@�`@Ĝ@r�@A�@ �@b@�@��@�@�P@|�@K�@�@�R@��@�+@v�@E�@@�h@?}@�@�/@��@z�@j@j@j@Z@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AᛦA��A��A��A�A�A�A�A�-A�-A�!A�A�A�!A�!A�-A�-A�-A�-A�9A�9A�9A�9A�FA�FA�FA�RA�RA�^A�^A�RA�FA�7LA�l�A���AۃA�-A���A�ffA�1A�dZA�JA�7LA�  AѸRA�9XA�5?A�p�A͙�A�^5A�5?A�{AǛ�AƃA�\)A�\)A©�A�  A�A��yA��A�I�A�ƨA�ffA�G�A�^5A��jA��A�x�A�G�A���A�VA��A� �A�^5A�A�A�p�A���A�A�VA��wA��+A��A��hA���A�VA�{A��HA�9XA��DA��TA�1A��
A�~�A��mA�{A���A���A�l�A�`BA���A�\)A�A�bA���A���A�v�A�7LA���A�&�A�1A��!A�XA�p�AƨAS�A|jAx��AwO�AvffAu"�As�ArE�AqoAnv�Am��AmhsAl-Aj��Ai�#Ag�Ac7LAa?}A^�`A]��A\��A\VAY�AX�HAW��AU�
AS33AR�AQS�AO�TAN5?AI|�AHffAG��AG�PAG�AE�
AE&�AC�AB�DAA�A@bNA@A�A@9XA?�#A?33A<��A:~�A:(�A9x�A8^5A8(�A7ƨA7VA5�7A2-A0�\A/�^A/�A-|�A,VA,JA+��A*��A*~�A*(�A(�`A&��A%�A$�+A#7LA ȴA��AbA�PA`BA�A�Av�AA�jA��A��A��A�A
=AȴAv�A��AO�A�A1'A-A33A��A��AĜA��AffA
n�A%A�AƨA&�A�A��A�AA�A�A �@�;dA �D@��@�j@�&�@�%@�%@�?}@��+@�hs@�w@�hs@��-@�+@�=q@�p�@�&�@�bN@�(�@��@���@�dZ@��@�E�@�V@�o@���@ڗ�@ؼj@�  @��@�S�@��@�|�@�t�@�dZ@�x�@�p�@���@؋D@�`B@ڇ+@��@�V@�+@�|�@���@���@և+@�G�@��m@�5?@�O�@Гu@��m@���@�{@�hs@˥�@�ȴ@�^5@�J@��@Ǿw@���@��T@�r�@�1@���@��m@��@�ff@�E�@�@���@���@�X@��@��@�"�@�^5@��7@�%@�t�@�^5@�5?@���@�o@�C�@���@�^5@��#@�p�@�p�@�G�@��@�&�@�/@�hs@�z�@�dZ@�ȴ@�dZ@���@��m@��w@���@�ff@��@��-@���@���@�G�@�V@��@�7L@�7L@�/@��@���@�b@��
@���@��@���@��P@��@�@���@���@�V@�%@�&�@�/@�A�@��m@�l�@�@�ȴ@��@���@�V@��h@��@���@�?}@���@��u@�z�@�(�@���@���@��w@���@���@�  @��m@���@���@�x�@�V@��m@��P@�t�@��@�n�@�$�@�$�@��@�/@��/@��9@���@�Z@�I�@�9X@��@��F@���@�"�@�=q@��7@�%@��j@�z�@��@��@�v�@�$�@��@�@��7@�`B@�&�@��@�Z@�b@���@��w@�ƨ@���@��w@�C�@��@��@��+@��^@��@�J@��@�@��7@�x�@�x�@�X@���@�%@��@��`@��D@�z�@�j@�Q�@�I�@�A�@�9X@�(�@�1'@��@��m@��F@���@��@�S�@�+@�
=@���@�^5@�E�@�=q@��@��#@���@�O�@��@�%@��@���@��@� �@��@�l�@�C�@�o@���@���@�~�@�5?@�@��#@��#@���@�@��7@��@�G�@���@��9@�Q�@�I�@�1@�ƨ@���@��@�l�@�S�@�33@��y@���@��\@�~�@�^5@��@�p�@�&�@���@���@���@�r�@� �@���@�l�@�C�@���@�n�@�-@�5?@���@�G�@���@��@�
=@��@��y@��!@�~�@�-@��@���@�X@��@���@��`@���@�1@�ƨ@��P@�\)@�S�@�;d@�
=@�ȴ@�n�@�5?@�@�@�`B@�?}@��@��j@��9@��9@���@��@�bN@�b@�b@�@�@K�@�@~�@~��@~ff@~V@~5?@}�T@}@}�@}/@}V@|�/@|(�@{�F@{��@{�@{C�@{o@z�!@zn�@z�@y�#@yx�@y�@xĜ@x�@x  @w�P@vȴ@u�@t�@tZ@t1@s�@r��@q��@qhs@q&�@p�@o�;@ol�@n�+@n@m�-@m�h@m?}@mV@l�@l�@l�/@l��@l��@l�@l�D@l9X@k�
@k��@kC�@j��@jM�@i��@i��@ix�@h��@h��@hr�@hA�@g��@g
=@fȴ@f�+@f5?@e�@d�/@d�j@d��@dj@dI�@c�
@cS�@c"�@co@c@b��@b~�@a�7@`�u@`  @_|�@_�@^�@^v�@^V@^{@]��@]`B@\��@\�D@\9X@[�@Z�\@Y��@Y�7@YX@XĜ@XQ�@W�P@W+@V��@Vȴ@V��@V$�@U��@U�h@T�/@TI�@T�@S�m@S��@S33@So@R��@Q�@P��@P�9@P�u@PbN@P1'@O�@Ol�@N��@Nff@N{@M�T@M@M��@M�h@Mp�@L��@L��@L�@Lz�@L(�@KC�@J��@J�!@J�!@J~�@JM�@J�@I��@I�7@H��@Hr�@HA�@H �@G�P@G+@F��@F�@F�@Fȴ@F��@E`B@C�m@CdZ@C"�@B�@B��@B��@B��@B�!@B��@B�\@BM�@BM�@B=q@A��@AX@@��@@��@@ �@?��@?l�@?�@>��@>ȴ@>��@>ff@>V@>V@>E�@>$�@=��@=?}@<��@<z�@<(�@<1@;�
@;ƨ@;�F@;��@;S�@;@:��@:��@:��@:�!@:~�@:~�@:~�@:~�@:^5@9��@9��@9X@9X@9X@9G�@9&�@8�9@8�@8r�@8bN@8A�@8  @7��@7|�@7\)@7�@6ȴ@6�R@6�R@6��@6ff@6E�@6{@5�T@4�/@4z�@4I�@3�
@3�@333@3o@2�H@2�\@2�@1��@1G�@17L@1&�@0�`@0�u@01'@/�;@/��@/l�@/+@/
=@.ȴ@.V@-��@-�@-?}@,�@,�@,Z@,�@+��@+C�@+@*M�@)�#@)x�@)�@(�`@(Ĝ@(bN@'��@'��@'|�@'K�@'+@&�@%�@%@%@%�-@%�h@$�@$��@$�/@$�/@$�/@$�@$�/@$�@$Z@#ƨ@#dZ@#o@"�\@"M�@"=q@"�@"J@!��@!�@!�^@ ��@ �u@ bN@ Q�@ A�@  �@�@�P@
=@�+@$�@�@��@�h@�h@�h@�@p�@/@�@��@�@�@�@��@�j@�@��@z�@(�@��@�m@ƨ@S�@33@o@@@@��@�\@M�@��@��@��@x�@%@Ĝ@�9@�@A�@�;@�w@�P@�P@|�@l�@\)@K�@;d@+@+@��@ȴ@��@V@�T@��@�@O�@/@�@��@�j@��@�D@j@I�@1@�
@ƨ@t�@��@��@��@�\@n�@M�@J@�@�^@hs@7L@�@%@�`@Ĝ@r�@A�@ �@b@�@��@�@�P@|�@K�@�@�R@��@�+@v�@E�@@�h@?}@�@�/@��@z�@j@j@j@Z@I�@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB�B~�B��B��B�B�9B�9B�-BÖB��B��B��B�B=qBK�BXBdZBt�B�VB��B��B�B�^B�3B��B�bB�B��B��BbNB[#BXBXBT�BhsB[#Bl�B��B�B�!B�5B�B��BŢB�FB��Bv�BZBcTBYB:^BA�B7LB,BoB�B�`B�/B��B�jB��B��B��B�1B�B~�Bn�BZBG�B-B{BB
��B
�B
��B
�^B
��B
��B
��B
�uB
�B
jB
`BB
\)B
N�B
5?B
+B
"�B
�B
�B
�B
hB
B	��B	��B	�B	�B	�mB	�;B	�dB	�B	��B	��B	�hB	�\B	�B	{�B	x�B	o�B	`BB	YB	S�B	L�B	B�B	)�B	"�B	 �B	�B	�B	�B	oB	PB	%B	  B��B��B��B��B��B�B�`B�TB�HB�5B�#B�B��B��B��B�jB�RB�FB�-B�B��B��B��B��B��B��B��B��B�{B�hB�PB�1B�%B�B�B�B�%B�B�B� B~�Bz�Bz�Bx�Bw�Bw�Bv�Bv�Bu�Bu�Bu�Bw�By�Bz�B|�B�%B�hB�uB�+Bp�BjBiyBs�By�B�B�B�DB�%B�+B�hB�B��B�JBx�Bw�Bz�B~�B��B�oB�PB�B~�Bw�By�Bv�Bx�Bz�B�%B�B�B�B~�B}�B{�Bu�Bp�Bm�Bo�Bq�Bu�By�B�7B��B��B�!B�?B�RB�^B�XB�wB��B�B�5B�B�5B�/B�mB��B��B	B	B	B	B	B	B	B	B		7B		7B		7B	
=B	VB	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	$�B	&�B	'�B	+B	-B	0!B	8RB	8RB	6FB	49B	1'B	/B	1'B	5?B	;dB	A�B	I�B	L�B	O�B	R�B	T�B	W
B	[#B	`BB	bNB	e`B	cTB	aHB	bNB	ffB	iyB	k�B	k�B	k�B	l�B	m�B	r�B	u�B	w�B	y�B	x�B	z�B	{�B	|�B	}�B	�B	�B	�%B	�B	�%B	�+B	�DB	�JB	�hB	�uB	�\B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�#B	�)B	�5B	�5B	�;B	�;B	�BB	�TB	�ZB	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
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
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
PB
VB
\B
\B
bB
bB
hB
hB
oB
oB
oB
hB
hB
bB
oB
oB
hB
\B
VB
JB
PB
\B
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
$�B
%�B
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
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
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
33B
33B
33B
33B
49B
5?B
6FB
6FB
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
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
F�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
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
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
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
cTB
cTB
dZB
dZB
e`B
e`B
ffB
ffB
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
hsB
iyB
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
jB
jB
jB
jB
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
n�B
n�B
n�B
n�B
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
q�B
p�B
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
s�B
s�B
s�B
s�B
s�B
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
w�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�BBBBBBBBBBBBBBBBBBBBBBBBBBBB'B;B�B{B#B�B��B�"B�iB�B��B��BāBB�2B�"B�B?HBM�BZQBg�Bx�B��B��B�tB��B��B��B��B�[B��B��B�~Bc�B\�BZ�B[�BV�Bj�B]dBm�B�B�"B�[BߤB�7B��BɠB��B��BzDB\�Bh
B]dB<PBD3B;JB0oB�B�ZB�8B�pB��B�cB�6B��B�?B��B��B��Bp�B\)BKB0!B
B�B
��B
�B
��B
�VB
�$B
�]B
�7B
��B
��B
lB
a�B
_�B
R�B
6�B
,WB
$�B
�B
 vB
~B
�B
�B	��B	�fB	��B	�"B	�B	��B	�(B	��B	�HB	��B	��B	��B	��B	}�B	{dB	raB	a�B	Z�B	VB	O�B	G+B	+QB	#�B	!|B	�B	/B	�B	B	B	�B	 �B�6B�6B��B��B��B��B�2B�ZB�B޸B��BچB׍B�{BÖB��B��B�8B�hB��B��B��B��B��B��B�0B�bB��B��B�aB��B�B��B��B�9B�B�+B�MB�gB�aB�iB|B{By$BxRBxlBw�Bw�Bv�BwLBxBx�Bz^B{0B}B��B��B��B�Br�Bl�BjKBtBz^B��B��B��B��B�KB�hB�/B��B�By>Bw�Bz�B~�B�SB��B�(B�tB��Bx�Bz^Bw2ByrB{�B�zB��B��B��B�BB}qBwLBraBn�BpBq�Bu�Bx�B�B�B�`B�'B�tB��B�^B��B�BB�[B��B�!B�7BޞB�dB�
B��B��B	B	�B	�B	�B	�B	�B	�B	9B		�B		�B		�B	)B	(B	,B	?B	YB	�B	�B	�B	IB	"B	#B	#B	%B	'8B	(>B	+QB	-�B	1'B	8�B	8�B	6�B	5?B	1�B	/5B	0�B	5B	;B	A�B	J	B	M6B	P.B	SB	U2B	W?B	[#B	`\B	b�B	e�B	dB	a�B	a�B	f2B	i�B	k�B	lB	k�B	l�B	m�B	r�B	u�B	xB	y�B	x�B	z�B	|B	}B	~(B	�oB	��B	�YB	�SB	�?B	�+B	�^B	�~B	�B	�FB	��B	�vB	��B	��B	��B	�ZB	�&B	�ZB	�,B	�2B	�B	�yB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	��B	�6B	ϑB	ϑB	�<B	ϑB	�<B	�B	�BB	�(B	�B	�B	�[B	�9B	�?B	�=B	�CB	�jB	�OB	�VB	�pB	��B	�B	��B	�B	��B	��B	�B	��B	��B	�=B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�+B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	�"B	�B	�B	�(B	�.B	�.B
 B
 4B
;B
'B
'B
'B
[B
aB
GB
3B
3B
3B
MB
MB
gB
mB
SB
SB
SB
tB
_B
zB
zB
_B
KB
1B
fB
KB
fB
	RB
	lB
	lB
	lB
	lB
	lB

rB

rB
	RB
	RB
	RB
	RB

XB
xB
xB
^B
xB
xB
�B
�B
�B
�B
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
B
�B
~B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
B
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
#B
"�B
"�B
#B
$B
$&B
%FB
&B
&B
&2B
&2B
'8B
'8B
'B
(
B
($B
($B
($B
(>B
)*B
)B
*B
*B
*B
*B
+B
+B
+B
+B
+B
+B
+6B
+6B
,"B
,=B
-]B
-)B
-)B
./B
./B
.IB
./B
/5B
/5B
/OB
0UB
0;B
0;B
0UB
0oB
1[B
2GB
2aB
2GB
2GB
2aB
2aB
3MB
3MB
3MB
3MB
3hB
3�B
4�B
5tB
6zB
6`B
6`B
6`B
6`B
6`B
7�B
7fB
7�B
7fB
7�B
7�B
7�B
8�B
8�B
9rB
9�B
9�B
:�B
:xB
;B
;B
;B
;�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
G+B
IB
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
KB
J�B
J�B
J�B
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
MB
NB
N"B
N�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
PB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
QB
Q B
P�B
P�B
Q B
QB
Q B
Q B
P�B
QB
Q B
RB
RB
RB
R B
RB
SB
R�B
R�B
SB
SB
S&B
SB
S&B
S@B
T,B
TB
T,B
UB
UB
UB
UB
VB
VSB
VSB
VB
VB
W$B
W$B
W?B
W?B
X+B
X+B
XEB
Y1B
Y1B
Y1B
YKB
YKB
Y1B
YKB
Z7B
Z7B
ZQB
Z7B
[qB
[=B
[WB
\]B
\xB
]IB
]dB
]IB
]IB
]dB
]dB
^OB
^OB
^OB
^OB
_pB
_�B
`\B
`BB
`vB
`\B
`vB
`\B
`BB
`BB
`BB
`\B
`\B
`\B
`vB
`vB
a|B
abB
a|B
bhB
bNB
bhB
bNB
bNB
bhB
bhB
b�B
cnB
c�B
cnB
cnB
cnB
cnB
c�B
d�B
d�B
ezB
e�B
f�B
f�B
f�B
ffB
gmB
g�B
g�B
gmB
g�B
gmB
hsB
hsB
h�B
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
jB
jB
j�B
j�B
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
n�B
n�B
n�B
n�B
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
q�B
p�B
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
s�B
s�B
s�B
s�B
s�B
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
w�B
xB
w�B
w�B
w�B
x�B
x�B
w�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608260033462016082600334620160826003346201806221301072018062213010720180622130107201804050700112018040507001120180405070011  JA  ARFMdecpA19c                                                                20160822093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160822003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160822003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160822003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160822003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160822003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160822003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160822003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160822003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160822003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20160822012102                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160822153621  CV  JULD            G�O�G�O�F�'�                JM  ARGQJMQC2.0                                                                 20160822153621  CV  JULD_LOCATION   G�O�G�O�F�'�                JM  ARGQJMQC2.0                                                                 20160822153621  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20160822153621  CV  LONGITUDE       G�O�G�O��$�%                JM  ARCAJMQC2.0                                                                 20160825153346  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160825153346  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220011  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040107  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                