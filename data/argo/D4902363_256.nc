CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:36:07Z creation;2018-07-23T03:36:11Z conversion to V3.1;2019-12-19T07:38:21Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180723033607  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_256                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�o5t��1   @�o6I���@9���$tT�dH$�/�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A^ffA�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�\)@�\)A�A?�A^{A�A��
A��
A���A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C{C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�HCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��DxRD�RD ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$�RD%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9�RD:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH�RDI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DYDY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��)D�<)D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AЮAа!Aа!AЩ�AЩ�AУ�AУ�AХ�AЧ�AЧ�AХ�AЇ+A��A���A���Aɲ-A���A�VA¾wA�hsA���A��A�ȴA���A�A�ȴA���A�O�A��A���A��A��RA�7LA���A��A��jA��A�JA�bA�E�A���A���A�1'A���A�t�A�{A�`BA�?}A���A�A��A���A��A��9A��HA�z�A��wA�ffA��A�I�A��wA�v�A�
=A���A�(�A�\)A�C�A���A��A�r�A�A�A��
A��#A�=qA��A�~�A� �A�
=A�$�A�A�|�A��A���A��A��RA���A� �A��`A��A�A
=A~I�A}S�A|ffAyO�Aw�7Av��Au�Asl�Aq&�Ao��AnĜAl��Ak�
AkK�Ai��Ah�Agt�Af�RAf{Aex�Ad�!AchsAb��Ab�9AaO�A`�!A`~�A_K�A^��A]+A\��A\Q�A[�
A[t�AZ�`AZȴAZ�AW�hAWoAVffAU�7AT$�AQ�mAOAMx�AL��AL�uAK�AK
=AJ�jAJz�AI�hAHJAG7LAF��AE��AD^5AC;dAB��AB^5AB1AA��AA&�A@�A@^5A@�A?�-A>�\A=ƨA<A�A;��A:(�A9��A9��A8ZA7�PA6��A61'A5��A4�A3�A3l�A1?}A/�TA/p�A-�wA+�^A+��A+x�A+"�A*�uA)�A(��A(5?A'��A'|�A';dA&��A%�mA%��A%��A%C�A$��A$M�A#��A"�A"��A!�hA jA7LA�A�#A�AjA�+A(�AQ�A�mA��A+AE�A"�A\)A&�A��A �At�A�+A$�A�hAjA�PA`BA33A
��A
 �A	��A	�A	|�AQ�At�A%A�HA�\A�FA"�A~�AO�A5?AA�A�TAO�A �\@�t�@��9@�b@��7@��w@��+@�J@��
@�7L@�Q�@��@��@�X@���@���@��@�-@��/@��m@��@�\)@��@�&�@㕁@��#@���@�`B@ۮ@�{@�1'@��#@��@�r�@�S�@�^5@�5?@���@�Ĝ@�b@θR@ͺ^@̛�@̃@�1'@˅@�/@�
=@�v�@Ų-@��@���@���@�(�@�|�@��y@�?}@�1'@�"�@�ff@��@�Q�@�dZ@�33@��@���@��\@�E�@���@��/@���@��@�J@��j@�(�@�|�@�;d@�-@�r�@��P@�J@��@�Z@�;d@���@�  @���@��@�&�@��@�Q�@�(�@��@���@�J@�hs@��9@�1@��m@��@�S�@�
=@�n�@��@���@���@���@��@��@���@�Ĝ@��
@���@�@��7@���@�b@��R@��@�O�@��@���@���@��@��/@�Ĝ@���@��@�1@�33@��R@�^5@�E�@�$�@���@�G�@���@�1'@�ƨ@���@���@�\)@��y@��+@�J@��@���@�bN@�9X@���@��@���@���@���@��@�l�@�+@���@���@�5?@���@��#@���@��^@��-@�p�@�1@�S�@�o@�@���@��T@��@�&�@��@�r�@� �@���@�t�@�dZ@�C�@�+@�+@�o@���@��H@�ȴ@���@���@��+@�V@��#@�@���@��7@�G�@��@�%@���@���@���@�Ĝ@��j@���@�bN@�1'@~��@}?}@|��@|9X@|1@{C�@z�\@z=q@y��@y�7@yG�@x�`@w��@w|�@w�@v��@vff@vE�@v{@u��@uO�@t�@sƨ@s"�@r��@r�\@r^5@r-@rJ@q��@q��@qhs@q�@pĜ@pQ�@pb@o��@o\)@n�R@nff@n@m�T@m@mO�@j�H@i�#@i��@i�^@i��@ihs@i7L@h��@h�9@hbN@hQ�@hQ�@hQ�@hA�@h �@h  @g�@g�P@g|�@gl�@g\)@gK�@g+@f��@fȴ@f�+@f$�@f@f@f@f@e�@e��@e��@eV@d�j@dI�@cS�@co@c"�@c33@c@b��@bM�@bJ@a��@a%@`Q�@_��@_|�@^�R@^5?@]�T@]@]��@]p�@]/@]V@\�D@\Z@\1@[o@Z�H@Z��@Z��@Z��@Z=q@Y�#@Y��@Yx�@Yx�@Yhs@Y&�@X�`@XbN@W��@W�@V��@Vff@VE�@U��@U�h@Up�@U/@T�@T��@Tz�@T9X@S��@S�@SS�@S33@R��@R��@R~�@Rn�@R�@Q�#@Qhs@Q�@P��@PĜ@P�@PbN@PQ�@O�w@O+@N5?@M@M�@L��@L(�@Kƨ@Kt�@KS�@K"�@K@J��@J�@JJ@I��@I�#@Ihs@H��@HQ�@H  @G�;@G�w@GK�@F�R@Fv�@F5?@F5?@F5?@F$�@F{@F@E�T@E@E`B@D�D@C��@C"�@B�@B�!@Bn�@B=q@BJ@A�#@Ax�@@��@@r�@@A�@@1'@@b@@b@@  @?�;@?��@?�P@?;d@?�@>��@>V@>E�@>@=��@=p�@=/@=V@<�@<z�@<9X@<�@;�
@;�@;t�@;33@:��@:�\@:^5@:-@:J@9hs@8��@8�u@8A�@7�;@7l�@7
=@6ff@5�T@5p�@5`B@5?}@5V@4��@4z�@49X@3��@333@3o@3@3@3@2�@2�H@2�\@2~�@2n�@2^5@2M�@1��@1�#@1�^@1��@17L@0�u@0bN@01'@/��@/;d@.�@.�R@.��@.��@.v�@.E�@-�h@-?}@-V@,��@,z�@,I�@,�@+�m@+��@+dZ@+C�@+"�@*�@*��@*M�@)��@)�#@)��@)�^@)��@)��@)�7@)G�@)7L@)&�@)�@)%@(��@(��@(�@(b@'�w@'l�@'+@'
=@&��@&��@&�y@&E�@%�@$�j@$�j@$�@$z�@$Z@$I�@$�@#�
@#ƨ@#�F@#��@#��@#�@#t�@"�@"~�@"^5@"M�@"=q@"=q@"=q@"=q@"J@!��@!��@!x�@!G�@!&�@ Ĝ@ �@ bN@ Q�@  �@��@�w@��@\)@ȴ@5?@�@�@O�@/@V@�/@�/@��@�j@�@�D@Z@1@�@"�@�@��@�!@��@�\@~�@-@��@�@�#@�^@��@x�@G�@%@�`@�9@��@\)@�y@��@5?@@�@�T@��@@�-@?}@�/@�j@�D@j@��@t�@33@�@�!@n�@^5@-@��@�@��@��@��@X@�`@�`@�`@�9@�@�@�@bN@1'@�@��@K�@�y@��@�@`B@/@V@V@�@�@�@�/@��@�j@�j@��@j@I�@9X@(�@�@��@�
@��@��@��@��@��@�@t�@S�@@
�H@
�!@
��@
=q@	�#@	��@	�7@	X@	&�@��@��@Ĝ@�9@�u@bN@Q�@ �@��@�P@|�@|�@|�@K�@+@�@�R@��@�+@v�@V@E�@5?@�T@��@p�@O�@/@V@�@�/@��@�@��@z�@z�@j@I�@9X@9X@9X@(�@1@�F@�@t�@�@dZ@@��@�!@��@~�@��@��@x�@G�@7L@%@ �u@ A�@  �?��w?�|�?�;d?���?��?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AЮAа!Aа!AЩ�AЩ�AУ�AУ�AХ�AЧ�AЧ�AХ�AЇ+A��A���A���Aɲ-A���A�VA¾wA�hsA���A��A�ȴA���A�A�ȴA���A�O�A��A���A��A��RA�7LA���A��A��jA��A�JA�bA�E�A���A���A�1'A���A�t�A�{A�`BA�?}A���A�A��A���A��A��9A��HA�z�A��wA�ffA��A�I�A��wA�v�A�
=A���A�(�A�\)A�C�A���A��A�r�A�A�A��
A��#A�=qA��A�~�A� �A�
=A�$�A�A�|�A��A���A��A��RA���A� �A��`A��A�A
=A~I�A}S�A|ffAyO�Aw�7Av��Au�Asl�Aq&�Ao��AnĜAl��Ak�
AkK�Ai��Ah�Agt�Af�RAf{Aex�Ad�!AchsAb��Ab�9AaO�A`�!A`~�A_K�A^��A]+A\��A\Q�A[�
A[t�AZ�`AZȴAZ�AW�hAWoAVffAU�7AT$�AQ�mAOAMx�AL��AL�uAK�AK
=AJ�jAJz�AI�hAHJAG7LAF��AE��AD^5AC;dAB��AB^5AB1AA��AA&�A@�A@^5A@�A?�-A>�\A=ƨA<A�A;��A:(�A9��A9��A8ZA7�PA6��A61'A5��A4�A3�A3l�A1?}A/�TA/p�A-�wA+�^A+��A+x�A+"�A*�uA)�A(��A(5?A'��A'|�A';dA&��A%�mA%��A%��A%C�A$��A$M�A#��A"�A"��A!�hA jA7LA�A�#A�AjA�+A(�AQ�A�mA��A+AE�A"�A\)A&�A��A �At�A�+A$�A�hAjA�PA`BA33A
��A
 �A	��A	�A	|�AQ�At�A%A�HA�\A�FA"�A~�AO�A5?AA�A�TAO�A �\@�t�@��9@�b@��7@��w@��+@�J@��
@�7L@�Q�@��@��@�X@���@���@��@�-@��/@��m@��@�\)@��@�&�@㕁@��#@���@�`B@ۮ@�{@�1'@��#@��@�r�@�S�@�^5@�5?@���@�Ĝ@�b@θR@ͺ^@̛�@̃@�1'@˅@�/@�
=@�v�@Ų-@��@���@���@�(�@�|�@��y@�?}@�1'@�"�@�ff@��@�Q�@�dZ@�33@��@���@��\@�E�@���@��/@���@��@�J@��j@�(�@�|�@�;d@�-@�r�@��P@�J@��@�Z@�;d@���@�  @���@��@�&�@��@�Q�@�(�@��@���@�J@�hs@��9@�1@��m@��@�S�@�
=@�n�@��@���@���@���@��@��@���@�Ĝ@��
@���@�@��7@���@�b@��R@��@�O�@��@���@���@��@��/@�Ĝ@���@��@�1@�33@��R@�^5@�E�@�$�@���@�G�@���@�1'@�ƨ@���@���@�\)@��y@��+@�J@��@���@�bN@�9X@���@��@���@���@���@��@�l�@�+@���@���@�5?@���@��#@���@��^@��-@�p�@�1@�S�@�o@�@���@��T@��@�&�@��@�r�@� �@���@�t�@�dZ@�C�@�+@�+@�o@���@��H@�ȴ@���@���@��+@�V@��#@�@���@��7@�G�@��@�%@���@���@���@�Ĝ@��j@���@�bN@�1'@~��@}?}@|��@|9X@|1@{C�@z�\@z=q@y��@y�7@yG�@x�`@w��@w|�@w�@v��@vff@vE�@v{@u��@uO�@t�@sƨ@s"�@r��@r�\@r^5@r-@rJ@q��@q��@qhs@q�@pĜ@pQ�@pb@o��@o\)@n�R@nff@n@m�T@m@mO�@j�H@i�#@i��@i�^@i��@ihs@i7L@h��@h�9@hbN@hQ�@hQ�@hQ�@hA�@h �@h  @g�@g�P@g|�@gl�@g\)@gK�@g+@f��@fȴ@f�+@f$�@f@f@f@f@e�@e��@e��@eV@d�j@dI�@cS�@co@c"�@c33@c@b��@bM�@bJ@a��@a%@`Q�@_��@_|�@^�R@^5?@]�T@]@]��@]p�@]/@]V@\�D@\Z@\1@[o@Z�H@Z��@Z��@Z��@Z=q@Y�#@Y��@Yx�@Yx�@Yhs@Y&�@X�`@XbN@W��@W�@V��@Vff@VE�@U��@U�h@Up�@U/@T�@T��@Tz�@T9X@S��@S�@SS�@S33@R��@R��@R~�@Rn�@R�@Q�#@Qhs@Q�@P��@PĜ@P�@PbN@PQ�@O�w@O+@N5?@M@M�@L��@L(�@Kƨ@Kt�@KS�@K"�@K@J��@J�@JJ@I��@I�#@Ihs@H��@HQ�@H  @G�;@G�w@GK�@F�R@Fv�@F5?@F5?@F5?@F$�@F{@F@E�T@E@E`B@D�D@C��@C"�@B�@B�!@Bn�@B=q@BJ@A�#@Ax�@@��@@r�@@A�@@1'@@b@@b@@  @?�;@?��@?�P@?;d@?�@>��@>V@>E�@>@=��@=p�@=/@=V@<�@<z�@<9X@<�@;�
@;�@;t�@;33@:��@:�\@:^5@:-@:J@9hs@8��@8�u@8A�@7�;@7l�@7
=@6ff@5�T@5p�@5`B@5?}@5V@4��@4z�@49X@3��@333@3o@3@3@3@2�@2�H@2�\@2~�@2n�@2^5@2M�@1��@1�#@1�^@1��@17L@0�u@0bN@01'@/��@/;d@.�@.�R@.��@.��@.v�@.E�@-�h@-?}@-V@,��@,z�@,I�@,�@+�m@+��@+dZ@+C�@+"�@*�@*��@*M�@)��@)�#@)��@)�^@)��@)��@)�7@)G�@)7L@)&�@)�@)%@(��@(��@(�@(b@'�w@'l�@'+@'
=@&��@&��@&�y@&E�@%�@$�j@$�j@$�@$z�@$Z@$I�@$�@#�
@#ƨ@#�F@#��@#��@#�@#t�@"�@"~�@"^5@"M�@"=q@"=q@"=q@"=q@"J@!��@!��@!x�@!G�@!&�@ Ĝ@ �@ bN@ Q�@  �@��@�w@��@\)@ȴ@5?@�@�@O�@/@V@�/@�/@��@�j@�@�D@Z@1@�@"�@�@��@�!@��@�\@~�@-@��@�@�#@�^@��@x�@G�@%@�`@�9@��@\)@�y@��@5?@@�@�T@��@@�-@?}@�/@�j@�D@j@��@t�@33@�@�!@n�@^5@-@��@�@��@��@��@X@�`@�`@�`@�9@�@�@�@bN@1'@�@��@K�@�y@��@�@`B@/@V@V@�@�@�@�/@��@�j@�j@��@j@I�@9X@(�@�@��@�
@��@��@��@��@��@�@t�@S�@@
�H@
�!@
��@
=q@	�#@	��@	�7@	X@	&�@��@��@Ĝ@�9@�u@bN@Q�@ �@��@�P@|�@|�@|�@K�@+@�@�R@��@�+@v�@V@E�@5?@�T@��@p�@O�@/@V@�@�/@��@�@��@z�@z�@j@I�@9X@9X@9X@(�@1@�F@�@t�@�@dZ@@��@�!@��@~�@��@��@x�@G�@7L@%@ �u@ A�@  �?��w?�|�?�;d?���?��?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�=B�7B�=B�=B�DB�JB�JB�JB�DB�7B�Bt�BS�B�uBƨB�B�BBcTB�=B� Br�BN�B��B�jB�RB��Bt�Bs�B��B�{B��B�uB�7B�hB�hB�JB�B|�BgmBdZBR�BgmBo�BdZBYBE�BW
BC�B:^BH�B;dB�B��B�B��B�;B��BŢBɺBȴB�dB��B��B�JB�bBp�B7LB.B&�BDBPBVB�B1BB
�mB
�/B
�B
�TB
�B
�HB
�
B
ɺB
��B
cTB
t�B
� B
z�B
r�B
n�B
]/B
N�B
-B
 �B
+B
bB
DB	�B	�B	�B	�B	�5B	�)B	ƨB	ÖB	��B	ÖB	�}B	�dB	�9B	��B	�B	��B	��B	��B	��B	�PB	�1B	� B	�%B	�=B	�%B	�B	� B	~�B	q�B	VB	aHB	bNB	XB	D�B	:^B	2-B	+B	@�B	A�B	;dB	33B	8RB	5?B	(�B	�B	�B	�B	PB	  B	B	DB	PB	
=B	1B	B	B��B��B��B�TB�NB��B�B��B��B�BȴBĜBBB��B�FB�LB�-B��B�{B��B�\B�B��B��B��B��B�\B�PB��B��B��B��B�uB�DB�oB��B�VB�B�B� By�B{�Bm�BcTB`BB_;BhsB]/BF�B0!B&�B9XBN�BR�BF�B;dB0!B)�BD�BA�B9XB6FB2-B;dB5?B)�B0!B@�B?}B8RB8RB=qB<jB6FB"�B+B2-B6FB.B!�B"�B!�B�B�B2-B1'B.B"�B�B�B
=B�BPB{B�B�BVBDB�B�B�B%�B%�B!�B �B�B"�B�B%�B"�B�B\BhBPBDB+B
=B	7BJB%B�B�B�B�B'�B$�B�B �B�B�B �B+B$�B�B\B{B,B)�B0!B1'B2-B)�B'�B%�B�B&�B(�B-B)�B33B49B>wB>wB>wB<jB:^B6FB2-B2-B33B8RB49B@�B?}BA�B:^B2-B@�B<jBC�BJ�BF�BD�BI�B[#BR�BaHBe`Bl�Bl�BhsBe`Bl�Bo�Bq�Bt�B|�B|�Bz�B|�Bz�B~�B�B�%B�B�B�B�+B�B|�Bz�B�+B�=B�7B�=B�JB�hB��B��B��B�B�B�B�B��B��B��B��B�'B�9B�RB�LB�?B�'B�?B�jB�}BĜBŢBB��BŢBƨBȴB��B��B�B�B�/B�BB�BB�;B�;B�5B�5B�;B�ZB�ZB�yB�B�B�B�B�B�`B�B��B��B��B��B	B	+B	
=B	
=B	bB	hB	�B	�B	�B	�B	 �B	�B	!�B	#�B	%�B	'�B	'�B	'�B	+B	,B	49B	49B	6FB	8RB	<jB	?}B	A�B	A�B	A�B	C�B	D�B	C�B	B�B	C�B	@�B	J�B	W
B	\)B	^5B	^5B	e`B	jB	l�B	m�B	o�B	o�B	n�B	t�B	u�B	v�B	y�B	z�B	z�B	y�B	{�B	{�B	}�B	�B	�+B	�=B	�DB	�JB	�PB	�VB	�\B	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�LB	�LB	�LB	�RB	�RB	�RB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�;B	�;B	�5B	�;B	�;B	�5B	�BB	�BB	�5B	�`B	�fB	�fB	�`B	�`B	�`B	�sB	�yB	�yB	�yB	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
%B
%B
%B
	7B
	7B
1B
1B
1B
1B
DB
JB
JB
DB
JB
VB
\B
hB
hB
hB
hB
hB
bB
bB
VB
JB
bB
bB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
 �B
�B
!�B
"�B
"�B
"�B
 �B
!�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
(�B
+B
+B
+B
+B
)�B
+B
)�B
,B
/B
/B
0!B
/B
/B
/B
.B
0!B
0!B
0!B
0!B
/B
/B
0!B
/B
.B
.B
1'B
1'B
/B
1'B
2-B
49B
49B
49B
49B
33B
1'B
33B
5?B
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
8RB
:^B
;dB
;dB
;dB
;dB
;dB
:^B
;dB
;dB
;dB
<jB
;dB
:^B
:^B
:^B
;dB
<jB
<jB
=qB
>wB
>wB
<jB
9XB
9XB
?}B
B�B
A�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
B�B
@�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
G�B
G�B
F�B
E�B
F�B
I�B
H�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
J�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
O�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
M�B
P�B
Q�B
S�B
S�B
VB
W
B
W
B
W
B
W
B
VB
T�B
VB
W
B
W
B
W
B
VB
W
B
YB
YB
YB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
^5B
^5B
]/B
]/B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
\)B
YB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
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
e`B
e`B
e`B
dZB
e`B
e`B
e`B
dZB
cTB
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
hsB
hsB
gmB
hsB
jB
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
l�B
l�B
m�B
m�B
m�B
n�B
m�B
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
n�B
n�B
o�B
q�B
r�B
q�B
q�B
r�B
s�B
s�B
r�B
q�B
r�B
s�B
s�B
t�B
s�B
r�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�=B�7B�=B�=B�DB�JB�JB�JB�^B�lB��Bv�BYB�sB�^B�B�BBg�B��B��Bv�BVB�B��B�$B��BzBxB�eB�?B�/B�9B�B��B�[B�VB�aBBj�Bf�BVSBhsBp;BezBZ�BH1BXBFB;�BIB<�B"�B�B��B��B�4B�9B�1B��BɆB��B��B�#B�B�Br�B;�B1'B)�BVB\B�B9B	lBAB
��B
�;B
� B
�tB
�	B
�|B
רB
��B
�B
iyB
w2B
�B
|6B
s�B
o�B
^�B
P�B
0oB
#B
+�B
�B
�B	�OB	�MB	�B	�qB	�;B	�B	ȴB	�B	��B	āB	�iB	�PB	�?B	�FB	��B	��B	�7B	�eB	�/B	��B	�7B	��B	��B	��B	��B	��B	��B	}B	r�B	X�B	bB	cTB	YeB	F�B	=<B	4�B	-�B	AB	BAB	<PB	49B	8�B	5�B	*KB	?B	�B	sB	�B	�B	[B	�B	�B	
�B	�B	�B	�B��B�qB��B��B�nB��B�7BΥBԯB�kB�=BżBðB�GB�oB��B�8B�MB�]B�9B��B��B��B��B�fB�\B��B��B�pB�1B�+B�B�B�FB�dB��B��B��B��B��B�Bz�B|�Bo5BeBa�B`�Bh�B^BH�B2�B)�B;dBO\BS[BG�B<�B1�B,"BD�BBAB:xB7LB3�B<B6FB+�B1AB@�B@ B9>B9	B=�B<�B6�B$tB,"B2�B6�B.�B#B#�B"�B#B�B2aB1vB.�B#�B�B�B�BIBB�BkB�BBB)B�B �B&fB&fB"�B!�B�B#TBpB&2B#:BIB�B�B�B�B�B^B
�B�B�B BOB~BpB($B%FB�B!|B�B�B!|B+QB%`B�BB�B,qB*�B0�B1vB2aB*�B(�B&�B�B'�B)�B-�B+B3�B4�B>�B>�B>�B<�B:�B6�B2�B2�B4B9	B5?B@�B@ BA�B;dB3�BAUB=�BDgBKDBG�BE�BJ�B[�BTFBa�Be�Bl�Bl�BiBf2BmBp!Br-Bu%B}B}<B{JB}<B{dBcB�SB�?B�SB�[B��B�EB�gB}�B{�B��B��B��B��B�PB�TB�B�$B�B�6B�B�B�B�*B�KB�mB��B�vB��B�lB��B��B��B��B��B��BĶB��B��B��B��B�+B�7B�0B�HB�1B�yB�IB�\B�\B�VB�VBބBބBߊB�B��B��B��B��B�B��B��B�B�B�"B�BB�jB�B	aB	zB	
�B	
�B	�B	�B	�B	�B	�B	�B	 �B	�B	!�B	#�B	%�B	(
B	(
B	($B	+6B	,WB	4nB	4nB	6zB	8�B	<�B	?�B	A�B	A�B	A�B	C�B	D�B	C�B	B�B	C�B	A;B	KDB	W?B	\]B	^jB	^�B	e�B	j�B	l�B	m�B	o�B	o�B	oB	t�B	u�B	v�B	y�B	z�B	z�B	zB	|B	|6B	~BB	�GB	�EB	�rB	�^B	�dB	��B	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�,B	�B	�IB	�/B	�/B	�5B	�;B	�AB	�GB	�?B	�LB	�LB	�fB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĜB	ĶB	ĜB	ðB	ðB	ªB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�BB	�B	�,B	�FB	�KB	�dB	�VB	�VB	�OB	�VB	�VB	ބB	�vB	�vB	ޞB	�`B	�B	�fB	�zB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�*B	�(B	�HB
UB
AB
3B
9B
?B
EB
?B
YB
tB
	7B
	RB
KB
�B
�B
�B
^B
dB
dB
�B
~B
pB
vB
hB
hB
�B
hB
hB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
 �B
 B
!�B
#B
"�B
"�B
!B
!�B
$B
$&B
%B
%B
&2B
&B
'8B
)B
+B
+6B
+6B
+6B
*0B
+6B
*KB
,"B
/5B
/B
0!B
/B
/B
/5B
./B
0!B
0!B
0;B
0UB
/5B
/OB
0UB
/5B
.IB
.IB
1AB
1AB
/OB
1AB
2GB
4TB
49B
4TB
4nB
3MB
1vB
3MB
5ZB
5tB
5ZB
6`B
6`B
6`B
6`B
7�B
7fB
7fB
7�B
7�B
7fB
8lB
:xB
;dB
;dB
;dB
;dB
;B
:xB
;dB
;dB
;�B
<jB
;B
:xB
:xB
:�B
;B
<�B
<�B
=�B
>wB
>wB
<�B
9�B
9�B
?�B
B�B
A�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
B�B
@�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
G�B
G�B
F�B
E�B
F�B
I�B
H�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
J�B
M�B
OB
OB
O�B
O�B
O�B
O�B
OB
O�B
P�B
Q�B
QB
Q B
Q B
Q B
Q B
Q B
O�B
N<B
QB
R:B
TB
TB
VB
W
B
W
B
W
B
W
B
VB
U2B
VB
W$B
W?B
W?B
V9B
W$B
Y1B
YKB
Y1B
Z7B
[#B
[=B
[#B
\)B
\CB
\CB
\CB
[=B
[=B
^5B
^5B
]IB
]IB
^5B
^OB
]dB
]IB
]IB
\]B
\]B
\]B
YB
`\B
abB
bhB
bhB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cnB
cnB
bhB
cnB
dZB
dZB
dZB
dtB
dtB
dtB
e`B
e`B
e`B
e`B
e`B
e�B
ezB
d�B
ezB
ezB
ezB
d�B
c�B
ezB
f�B
f�B
f�B
f�B
gmB
gmB
gmB
g�B
g�B
h�B
h�B
g�B
h�B
jB
jB
jB
i�B
i�B
i�B
j�B
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
m�B
m�B
m�B
n�B
m�B
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
n�B
n�B
o�B
q�B
r�B
q�B
q�B
r�B
s�B
s�B
r�B
q�B
r�B
s�B
s�B
t�B
s�B
r�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807080038522018070800385220180708003852201807080200172018070802001720180708020017201807090026482018070900264820180709002648  JA  ARFMdecpA19c                                                                20180723123521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033607  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033609  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033610  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033610  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033610  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033611  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033611  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723033611                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040126                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180704153426  CV  JULD            G�O�G�O�F�y�                JM  ARGQJMQC2.0                                                                 20180704153426  CV  JULD_LOCATION   G�O�G�O�F�y�                JM  ARGQJMQC2.0                                                                 20180704153426  CV  LATITUDE        G�O�G�O�A�V                JM  ARGQJMQC2.0                                                                 20180704153426  CV  LONGITUDE       G�O�G�O��"=�                JM  ARCAJMQC2.0                                                                 20180707153852  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180707153852  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180707170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180708152648  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                