CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-11-12T08:01:17Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20221112080117  20221112080117  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��S}��H1   @��T)���@+I�^5?�d�ȴ9X1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @���@�33@���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B~��B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@\@�(�AzA?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B�B�B Q�B'�B/�B7�B?�BG�BO�BW�B_�Bg�BpQ�Bx�RB~�RB���B���B���B�B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ�DJ��DK~�DK��DL~�DL��DM~�DM��DN�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\D�D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D낏D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��
A��#A��#A��/A��;A��HA��HA��TA��TA��TA��TA��TA��TA��`A��mA��mA��mA��yA��yA��yA��A��HA��;A���A��A�`BA�&�AԴ9A�A�7LA�hsAϧ�A� �A�^5A�1Aɉ7A�A�A�A�l�Aď\A¶FA���A��A�7LA�7LA��FA���A��A��yA���A�=qA��A���A��yA�Q�A�/A�
=A�A�A�Q�A��A�{A�C�A�oA�~�A�`BA��-A��jA��PA�33A��-A�9XA�  A���Az��AxAr��Ap�yAi��AdZAb��A]�mAT��ARA�AN~�ALbAG�wAD(�A?��A<�A;�A7�A6bA3��A1��A/�A-�^A,��A+/A)��A(��A(I�A'�wA'�7A'oA&v�A%p�A$�`A$�A#&�A"(�A!x�A �/A �!A jA 9XAƨAl�A��AZA��A��AI�A1A;dA�AbNAZAZA��AA��A(�A�A�A�!AjA1'AA�;A=qA�#AoAZAt�A��A�DA��AoAz�AI�A\)A�RAZA�AK�A��AM�A �At�AhsA?}A/A
��A
�A	�#A
�A�A`BA
�`A
��A
��A
z�A
�+A
JA	t�A��A��A��A$�A|�A�A�RAM�A{A�A�TA�;AƨA+A~�AbA��A��Ap�AVA�uAAXA�A �H@�C�@��@���@���@��@��@��@�^5@�J@���@�hs@�Ĝ@�A�@�t�@�"�@��y@��+@�5?@�`B@���@�1'@��@�^5@��@�?}@�1'@�S�@�+@�-@�{@���@�`B@��/@�  @�@�33@��@�R@ꗍ@�v�@�n�@�^5@�$�@��T@陚@�hs@��`@�Ĝ@�@�z�@�9X@�1@�@���@�=q@�^@��@���@���@�C�@��y@�E�@��@���@�1'@��;@��@�ff@��T@�`B@���@׮@���@�n�@ՙ�@�%@�t�@��@��@ҸR@Ұ!@ҏ\@�@ЋD@�  @�33@��y@θR@Χ�@Ώ\@�~�@�n�@�^5@�-@��#@�X@�Z@˕�@�o@���@�n�@�J@��@�@�x�@��@ȓu@�bN@�9X@�dZ@Ƨ�@Ə\@Ə\@�n�@��@�X@ēu@�Z@�b@�o@�E�@��@��-@�x�@��@��D@���@���@���@���@�E�@�J@��@���@��7@�X@�7L@�V@�V@���@��j@���@��D@� �@���@�+@��+@���@�p�@�&�@�z�@���@��+@�G�@��@��`@��@�1@���@��@���@��@��@���@���@��D@�bN@�I�@�(�@�b@���@���@���@�=q@��@��@��h@�/@��@�z�@�  @���@�dZ@���@�v�@�{@��@�@���@�?}@��`@��@�(�@���@���@��@�-@��^@��@��`@��9@��D@�z�@�Q�@���@��@��R@�ff@�V@�E�@��@��-@�hs@���@�Ĝ@��D@�I�@���@�dZ@��@�n�@��@��@�`B@��j@��u@��@�b@���@�+@��H@��!@�^5@��T@���@��-@��-@��-@��-@��^@��^@���@��@���@���@��y@�=q@���@���@���@�p�@�7L@�V@��@�Ĝ@��u@�bN@�A�@���@��y@���@�v�@��#@���@��@��9@��D@��@�bN@��
@��@�l�@���@��@���@�ȴ@���@��R@��!@��!@���@��\@�M�@�{@�@��7@��@�p�@�G�@���@���@�r�@�Q�@�9X@��@��
@���@�l�@�"�@�
=@�@��@���@�5?@���@�7L@�&�@�%@���@�Q�@�(�@��@�b@�1@�  @��F@�S�@���@��\@�n�@�5?@�@�G�@�&�@�Ĝ@�j@��@��
@�ƨ@��w@���@���@��P@�t�@�C�@�
=@��@�n�@�5?@��T@��@�?}@���@��j@�z�@�Q�@�(�@�  @��@�P@;d@~�@~�+@~V@}�@}O�@}�@|��@|�@{"�@z��@z��@z~�@y�@x��@w�;@w+@vE�@u�-@uO�@t�@tj@t1@s�
@s�F@sS�@r��@q�@q7L@pQ�@o��@o;d@o�@o�@o�@o
=@n�+@m�T@m�@m/@l��@l�@l��@lz�@l(�@k�m@k��@k33@j�!@jM�@i�@i�^@i&�@h��@h�@hbN@h1'@g�;@g�@g�@gK�@f�R@f�+@fE�@e�@e?}@eV@d��@d�@d�/@d��@dZ@d�@c�m@c��@ct�@cdZ@co@b-@a��@a%@`�@`r�@`r�@`r�@`r�@`Q�@`A�@` �@_��@_;d@_
=@^�@^��@^V@]��@]`B@]?}@]V@\��@\��@\�j@\��@\I�@\1@[�F@[S�@["�@[@Z=q@YX@X��@X  @W��@W��@W|�@W
=@V��@V�+@VE�@U��@U@U�-@UO�@U�@T��@T�@Tj@T1@S�m@S�
@S�
@Sƨ@S�@SS�@S33@R�@R�\@RJ@Q��@Q�^@Q��@Q�7@Qhs@PĜ@PQ�@P1'@P  @O�@Ol�@NV@M��@M�-@MV@L�@L�@Lj@L9X@Kƨ@KC�@J�H@Jn�@JM�@J�@I�@I�7@Ix�@I7L@H�`@H��@Hr�@H1'@G�@G�P@G;d@F��@Fff@FV@F5?@E�@E�-@EV@DI�@D�@D1@C�F@B�\@A�^@AX@A�@A%@@Ĝ@@bN@@A�@@ �@?�@?;d@>��@>5?@=p�@<�@<�@<��@<j@<1@;��@;dZ@:��@9�@9��@9hs@97L@8��@8A�@8  @6�y@6E�@5�@5�-@5��@5��@5p�@5`B@5`B@5?}@4�@4z�@49X@3t�@3o@2�@2��@2n�@2M�@2�@1x�@0��@/l�@/
=@.�R@.v�@.E�@-�T@-@-O�@,�@,j@,�@+��@+�
@+dZ@+@*�!@*��@*�\@*^5@*M�@)��@)��@)hs@)G�@)&�@)%@(��@(�`@(�u@(  @'�@';d@&ȴ@&��@&��@&�+@&E�@&@%?}@$��@$��@$z�@$1@#�
@#��@#�@#S�@#33@#"�@#o@#o@#o@#@#@"�@"��@"�\@"n�@"=q@"J@!�@!��@!x�@!�@ Ĝ@ �9@ �u@ r�@ r�@ A�@ b@�;@�@��@�P@l�@;d@
=@�R@��@v�@5?@$�@@��@��@@@@�-@��@�@`B@�@��@j@Z@I�@�@��@�m@�F@��@�@dZ@o@�@�@�H@�H@�H@��@��@�!@��@n�@J@�#@�^@��@x�@X@&�@�@Ĝ@bN@1'@  @��@�@|�@;d@
=@�@ȴ@��@��@�+@V@{@�T@@��@p�@O�@V@�j@�@�D@z�@j@Z@I�@9X@�@�m@��@dZ@33@�@��@��@�!@��@n�@M�@=q@-@J@��@�@��@�^@��@�7@hs@G�@7L@�@��@�9@r�@bN@bN@A�@ �@  @�;@�w@��@�P@�P@|�@|�@l�@;d@+@+@�@��@��@�+@�+@v�@V@$�@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144A���A���A���A���A���A��
A��#A��#A��/A��;A��HA��HA��TA��TA��TA��TA��TA��TA��`A��mA��mA��mA��yA��yA��yA��A��HA��;A���A��A�`BA�&�AԴ9A�A�7LA�hsAϧ�A� �A�^5A�1Aɉ7A�A�A�A�l�Aď\A¶FA���A��A�7LA�7LA��FA���A��A��yA���A�=qA��A���A��yA�Q�A�/A�
=A�A�A�Q�A��A�{A�C�A�oA�~�A�`BA��-A��jA��PA�33A��-A�9XA�  A���Az��AxAr��Ap�yAi��AdZAb��A]�mAT��ARA�AN~�ALbAG�wAD(�A?��A<�A;�A7�A6bA3��A1��A/�A-�^A,��A+/A)��A(��A(I�A'�wA'�7A'oA&v�A%p�A$�`A$�A#&�A"(�A!x�A �/A �!A jA 9XAƨAl�A��AZA��A��AI�A1A;dA�AbNAZAZA��AA��A(�A�A�A�!AjA1'AA�;A=qA�#AoAZAt�A��A�DA��AoAz�AI�A\)A�RAZA�AK�A��AM�A �At�AhsA?}A/A
��A
�A	�#A
�A�A`BA
�`A
��A
��A
z�A
�+A
JA	t�A��A��A��A$�A|�A�A�RAM�A{A�A�TA�;AƨA+A~�AbA��A��Ap�AVA�uAAXA�A �H@�C�@��@���@���@��@��@��@�^5@�J@���@�hs@�Ĝ@�A�@�t�@�"�@��y@��+@�5?@�`B@���@�1'@��@�^5@��@�?}@�1'@�S�@�+@�-@�{@���@�`B@��/@�  @�@�33@��@�R@ꗍ@�v�@�n�@�^5@�$�@��T@陚@�hs@��`@�Ĝ@�@�z�@�9X@�1@�@���@�=q@�^@��@���@���@�C�@��y@�E�@��@���@�1'@��;@��@�ff@��T@�`B@���@׮@���@�n�@ՙ�@�%@�t�@��@��@ҸR@Ұ!@ҏ\@�@ЋD@�  @�33@��y@θR@Χ�@Ώ\@�~�@�n�@�^5@�-@��#@�X@�Z@˕�@�o@���@�n�@�J@��@�@�x�@��@ȓu@�bN@�9X@�dZ@Ƨ�@Ə\@Ə\@�n�@��@�X@ēu@�Z@�b@�o@�E�@��@��-@�x�@��@��D@���@���@���@���@�E�@�J@��@���@��7@�X@�7L@�V@�V@���@��j@���@��D@� �@���@�+@��+@���@�p�@�&�@�z�@���@��+@�G�@��@��`@��@�1@���@��@���@��@��@���@���@��D@�bN@�I�@�(�@�b@���@���@���@�=q@��@��@��h@�/@��@�z�@�  @���@�dZ@���@�v�@�{@��@�@���@�?}@��`@��@�(�@���@���@��@�-@��^@��@��`@��9@��D@�z�@�Q�@���@��@��R@�ff@�V@�E�@��@��-@�hs@���@�Ĝ@��D@�I�@���@�dZ@��@�n�@��@��@�`B@��j@��u@��@�b@���@�+@��H@��!@�^5@��T@���@��-@��-@��-@��-@��^@��^@���@��@���@���@��y@�=q@���@���@���@�p�@�7L@�V@��@�Ĝ@��u@�bN@�A�@���@��y@���@�v�@��#@���@��@��9@��D@��@�bN@��
@��@�l�@���@��@���@�ȴ@���@��R@��!@��!@���@��\@�M�@�{@�@��7@��@�p�@�G�@���@���@�r�@�Q�@�9X@��@��
@���@�l�@�"�@�
=@�@��@���@�5?@���@�7L@�&�@�%@���@�Q�@�(�@��@�b@�1@�  @��F@�S�@���@��\@�n�@�5?@�@�G�@�&�@�Ĝ@�j@��@��
@�ƨ@��w@���@���@��P@�t�@�C�@�
=@��@�n�@�5?@��T@��@�?}@���@��j@�z�@�Q�@�(�@�  @��@�P@;d@~�@~�+@~V@}�@}O�@}�@|��@|�@{"�@z��@z��@z~�@y�@x��@w�;@w+@vE�@u�-@uO�@t�@tj@t1@s�
@s�F@sS�@r��@q�@q7L@pQ�@o��@o;d@o�@o�@o�@o
=@n�+@m�T@m�@m/@l��@l�@l��@lz�@l(�@k�m@k��@k33@j�!@jM�@i�@i�^@i&�@h��@h�@hbN@h1'@g�;@g�@g�@gK�@f�R@f�+@fE�@e�@e?}@eV@d��@d�@d�/@d��@dZ@d�@c�m@c��@ct�@cdZ@co@b-@a��@a%@`�@`r�@`r�@`r�@`r�@`Q�@`A�@` �@_��@_;d@_
=@^�@^��@^V@]��@]`B@]?}@]V@\��@\��@\�j@\��@\I�@\1@[�F@[S�@["�@[@Z=q@YX@X��@X  @W��@W��@W|�@W
=@V��@V�+@VE�@U��@U@U�-@UO�@U�@T��@T�@Tj@T1@S�m@S�
@S�
@Sƨ@S�@SS�@S33@R�@R�\@RJ@Q��@Q�^@Q��@Q�7@Qhs@PĜ@PQ�@P1'@P  @O�@Ol�@NV@M��@M�-@MV@L�@L�@Lj@L9X@Kƨ@KC�@J�H@Jn�@JM�@J�@I�@I�7@Ix�@I7L@H�`@H��@Hr�@H1'@G�@G�P@G;d@F��@Fff@FV@F5?@E�@E�-@EV@DI�@D�@D1@C�F@B�\@A�^@AX@A�@A%@@Ĝ@@bN@@A�@@ �@?�@?;d@>��@>5?@=p�@<�@<�@<��@<j@<1@;��@;dZ@:��@9�@9��@9hs@97L@8��@8A�@8  @6�y@6E�@5�@5�-@5��@5��@5p�@5`B@5`B@5?}@4�@4z�@49X@3t�@3o@2�@2��@2n�@2M�@2�@1x�@0��@/l�@/
=@.�R@.v�@.E�@-�T@-@-O�@,�@,j@,�@+��@+�
@+dZ@+@*�!@*��@*�\@*^5@*M�@)��@)��@)hs@)G�@)&�@)%@(��@(�`@(�u@(  @'�@';d@&ȴ@&��@&��@&�+@&E�@&@%?}@$��@$��@$z�@$1@#�
@#��@#�@#S�@#33@#"�@#o@#o@#o@#@#@"�@"��@"�\@"n�@"=q@"J@!�@!��@!x�@!�@ Ĝ@ �9@ �u@ r�@ r�@ A�@ b@�;@�@��@�P@l�@;d@
=@�R@��@v�@5?@$�@@��@��@@@@�-@��@�@`B@�@��@j@Z@I�@�@��@�m@�F@��@�@dZ@o@�@�@�H@�H@�H@��@��@�!@��@n�@J@�#@�^@��@x�@X@&�@�@Ĝ@bN@1'@  @��@�@|�@;d@
=@�@ȴ@��@��@�+@V@{@�T@@��@p�@O�@V@�j@�@�D@z�@j@Z@I�@9X@�@�m@��@dZ@33@�@��@��@�!@��@n�@M�@=q@-@J@��@�@��@�^@��@�7@hs@G�@7L@�@��@�9@r�@bN@bN@A�@ �@  @�;@�w@��@�P@�P@|�@|�@l�@;d@+@+@�@��@��@�+@�+@v�@V@$�@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B
�B
��B
��B
�9B
�B
��B
�NB
��B}�B�LB��BPB�B#�B#�B%�B�B�B#�B#�B��B�B�'B�3BȴB�+BcTBy�B�1B�+Bl�Bo�BffBF�B�B
��B
��B
��B
��B
��B
m�B
_;B
C�B
\B	�B	ĜB	��B	��B	}�B	w�B	D�B	�B	5?B	
=BÖB��B��B��B�B�mB�fB��B�NB��B�fB�B��B�B	B	�B	�B	.B	E�B	W
B	`BB	gmB	m�B	x�B	� B	�VB	�bB	�bB	�oB	��B	��B	�B	�B	�?B	�3B	�B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	B	ȴB	��B	ɺB	�dB	�B	�wB	ȴB	��B	��B	�B	�
B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�BB	�B	�B	�B	��B	��B
oB
$�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
"�B
 �B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
�B
hB
�B
�B
�B
�B
�B
{B
VB
uB
{B
hB
B
B
hB
VB
bB
hB
\B
\B
oB
oB
bB
PB
VB
PB
oB
uB
oB
oB
PB
PB
JB

=B
B
B
B
  B
B
B
1B

=B
	7B
+B
%B
B
	7B
1B

=B
JB
JB
PB
PB
PB
DB
DB
DB
PB
JB
VB
\B
VB
PB
PB
PB
DB
PB
PB
PB
VB
	7B

=B
	7B
B	��B	�B	�;B	�B	�B	�mB	�mB	�`B	�TB	�BB	�TB	�sB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B
B
B	��B
B
%B
+B
%B
B
B
B
+B
%B
1B
	7B

=B

=B
DB

=B
DB
JB
PB
\B
�B
�B
�B
�B
{B
{B
{B
{B
{B
�B
�B
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
 �B
!�B
"�B
$�B
&�B
&�B
%�B
$�B
%�B
$�B
'�B
'�B
'�B
'�B
'�B
(�B
+B
-B
.B
-B
,B
0!B
0!B
.B
-B
.B
/B
0!B
0!B
/B
33B
33B
49B
49B
49B
33B
33B
2-B
0!B
0!B
-B
.B
0!B
33B
49B
49B
49B
49B
5?B
5?B
49B
49B
33B
33B
1'B
0!B
5?B
49B
33B
5?B
5?B
5?B
8RB
9XB
7LB
5?B
8RB
8RB
7LB
:^B
;dB
<jB
;dB
<jB
;dB
;dB
;dB
:^B
9XB
9XB
:^B
;dB
<jB
;dB
;dB
9XB
;dB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
=qB
<jB
;dB
<jB
=qB
=qB
=qB
<jB
>wB
?}B
?}B
>wB
>wB
<jB
;dB
:^B
<jB
=qB
<jB
;dB
;dB
=qB
<jB
<jB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
?}B
?}B
>wB
?}B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
D�B
D�B
E�B
E�B
D�B
D�B
G�B
G�B
F�B
E�B
C�B
E�B
G�B
H�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
L�B
N�B
O�B
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
T�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
XB
W
B
W
B
W
B
XB
YB
XB
XB
ZB
YB
YB
YB
ZB
[#B
[#B
[#B
ZB
[#B
[#B
[#B
[#B
[#B
\)B
ZB
YB
ZB
[#B
\)B
^5B
^5B
^5B
^5B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
[#B
YB
YB
[#B
[#B
]/B
]/B
^5B
]/B
^5B
_;B
_;B
^5B
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
bNB
cTB
cTB
bNB
aHB
bNB
bNB
aHB
aHB
aHB
bNB
cTB
bNB
bNB
bNB
aHB
bNB
cTB
bNB
bNB
bNB
aHB
cTB
e`B
dZB
ffB
ffB
ffB
ffB
e`B
e`B
gmB
ffB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
jB
jB
jB
iyB
iyB
iyB
k�B
k�B
iyB
hsB
hsB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
m�B
l�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
m�B
o�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
r�B
q�B
s�B
s�B
r�B
t�B
u�B
u�B
t�B
u�B
u�B
s�B
s�B
s�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
{�B
{�B
|�B
~�B
~�B
~�B
~�B
}�B
|�B
~�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�%B
�%B
�%B
�+B
�+B
�%B
�+B
�+B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�+B
�+B
�1B
�1B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�JB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�oB
�hB
�hB
�hB
�hB
�hB
�oB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B
�B
��B
��B
�9B
�B
��B
�NB
��B}�B�LB��BPB�B#�B#�B%�B�B�B#�B#�B��B�B�'B�3BȴB�+BcTBy�B�1B�+Bl�Bo�BffBF�B�B
��B
��B
��B
��B
��B
m�B
_;B
C�B
\B	�B	ĜB	��B	��B	}�B	w�B	D�B	�B	5?B	
=BÖB��B��B��B�B�mB�fB��B�NB��B�fB�B��B�B	B	�B	�B	.B	E�B	W
B	`BB	gmB	m�B	x�B	� B	�VB	�bB	�bB	�oB	��B	��B	�B	�B	�?B	�3B	�B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	B	ȴB	��B	ɺB	�dB	�B	�wB	ȴB	��B	��B	�B	�
B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�BB	�B	�B	�B	��B	��B
oB
$�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
"�B
 �B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
�B
hB
�B
�B
�B
�B
�B
{B
VB
uB
{B
hB
B
B
hB
VB
bB
hB
\B
\B
oB
oB
bB
PB
VB
PB
oB
uB
oB
oB
PB
PB
JB

=B
B
B
B
  B
B
B
1B

=B
	7B
+B
%B
B
	7B
1B

=B
JB
JB
PB
PB
PB
DB
DB
DB
PB
JB
VB
\B
VB
PB
PB
PB
DB
PB
PB
PB
VB
	7B

=B
	7B
B	��B	�B	�;B	�B	�B	�mB	�mB	�`B	�TB	�BB	�TB	�sB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B
B
B	��B
B
%B
+B
%B
B
B
B
+B
%B
1B
	7B

=B

=B
DB

=B
DB
JB
PB
\B
�B
�B
�B
�B
{B
{B
{B
{B
{B
�B
�B
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
 �B
!�B
"�B
$�B
&�B
&�B
%�B
$�B
%�B
$�B
'�B
'�B
'�B
'�B
'�B
(�B
+B
-B
.B
-B
,B
0!B
0!B
.B
-B
.B
/B
0!B
0!B
/B
33B
33B
49B
49B
49B
33B
33B
2-B
0!B
0!B
-B
.B
0!B
33B
49B
49B
49B
49B
5?B
5?B
49B
49B
33B
33B
1'B
0!B
5?B
49B
33B
5?B
5?B
5?B
8RB
9XB
7LB
5?B
8RB
8RB
7LB
:^B
;dB
<jB
;dB
<jB
;dB
;dB
;dB
:^B
9XB
9XB
:^B
;dB
<jB
;dB
;dB
9XB
;dB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
=qB
<jB
;dB
<jB
=qB
=qB
=qB
<jB
>wB
?}B
?}B
>wB
>wB
<jB
;dB
:^B
<jB
=qB
<jB
;dB
;dB
=qB
<jB
<jB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
?}B
?}B
>wB
?}B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
D�B
D�B
E�B
E�B
D�B
D�B
G�B
G�B
F�B
E�B
C�B
E�B
G�B
H�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
L�B
N�B
O�B
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
T�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
XB
W
B
W
B
W
B
XB
YB
XB
XB
ZB
YB
YB
YB
ZB
[#B
[#B
[#B
ZB
[#B
[#B
[#B
[#B
[#B
\)B
ZB
YB
ZB
[#B
\)B
^5B
^5B
^5B
^5B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
[#B
YB
YB
[#B
[#B
]/B
]/B
^5B
]/B
^5B
_;B
_;B
^5B
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
bNB
cTB
cTB
bNB
aHB
bNB
bNB
aHB
aHB
aHB
bNB
cTB
bNB
bNB
bNB
aHB
bNB
cTB
bNB
bNB
bNB
aHB
cTB
e`B
dZB
ffB
ffB
ffB
ffB
e`B
e`B
gmB
ffB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
jB
jB
jB
iyB
iyB
iyB
k�B
k�B
iyB
hsB
hsB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
m�B
l�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
m�B
o�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
r�B
q�B
s�B
s�B
r�B
t�B
u�B
u�B
t�B
u�B
u�B
s�B
s�B
s�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
{�B
{�B
|�B
~�B
~�B
~�B
~�B
}�B
|�B
~�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�%B
�%B
�%B
�+B
�+B
�%B
�+B
�+B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�+B
�+B
�1B
�1B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�JB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�oB
�hB
�hB
�hB
�hB
�hB
�oB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221112080117                              AO  ARCAADJP                                                                    20221112080117    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221112080117  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221112080117  QCF$                G�O�G�O�G�O�4000            