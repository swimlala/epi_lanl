CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:36:20Z creation;2018-07-23T03:36:24Z conversion to V3.1;2019-12-19T07:37:54Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180723033620  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_258                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�p��o�1   @�p��I�@9��	��dLtS��M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(ffB/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
AУ�A��
A��
A��
B�B�B�B�B(Q�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��)D�<)D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�L)D�b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�O�A�M�A�M�A�M�A�I�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�M�A�G�A˃A�t�A�(�A�&�A�A��A��yA�1'A���A��A�bNA�hsA��A�1A�r�A�ȴA�=qA�$�A�\)A���A��A�\)A��-A�/A��PA��TA���A�S�A��
A�A���A�jA� �A�A�+A�ffA��^A�A�oA��^A�33A���A��#A���A��A�|�A��FA��A�^5A�dZA�A�5?A��uA��HA�5?A���A���A�VA��A�33A���A���A��A�K�A��A��A�=qA���A�;dA�|�A��A��9A�hsA���A�n�A�jAoA}S�AzbAw�Av��AuhsAtZAs��Aq��Ap��Ap^5Ao�Ao&�An�\AmƨAlffAk?}AjQ�Ai�hAh�Ah��AhVAh�Ag�7AgVAf��Ae��Ad9XAc7LAb��AaƨAa�A`�uA`Q�A_��A]�A\n�AZ�DAX�DAWoAV�AV=qAT~�ARr�AQhsAO�hAN��AM�AL��AKG�AJ=qAI��AH�RAF��AE��AD�AC�AC�wACdZAA�hA@��A@A>�yA>��A>9XA=XA<�A<=qA;�A:�A7�A6 �A5��A4ĜA4Q�A3��A3&�A3
=A2Q�A1+A0��A/A-�TA,bNA+VA)�A(�DA'��A&9XA%G�A$�\A"�A!|�A �RA\)AA7LA��A�A7LAI�At�A��Az�AA�A^5AAdZA�TA�\A�#AO�A�A��A��A�;AXA
Q�A	�-A	�A��AA�A�FA�AjA�A��AoA^5A��A��A��A�A ��A 5?A b@��
@���@���@��-@�b@��#@���@�Q�@��@��@�"�@���@�P@�$�@��@��@�"�@�E�@�{@��^@�h@�x�@��@�u@���@���@��@�z�@��@�|�@�X@�5?@�^@ᙚ@���@��m@��@���@��`@��@��T@�p�@ؼj@׍P@�33@�@�@�V@�@�&�@ϥ�@ΰ!@͑h@���@��@�M�@ȣ�@��y@�`B@���@�ƨ@�t�@�33@¸R@��h@��j@�dZ@���@���@��`@�ƨ@�$�@�bN@�o@���@�~�@���@�&�@��`@�9X@���@�;d@���@��7@��@���@�A�@��
@���@�;d@��H@��\@���@�S�@�x�@���@�r�@��@��@��P@��@�x�@�A�@��w@���@�"�@��@�bN@�l�@���@�-@�&�@��j@�A�@��m@�t�@�dZ@�S�@�@��!@��\@�n�@�^5@�^5@��@���@��@� �@��@���@�^5@��#@�`B@���@��@�Z@��@�|�@���@�V@��@���@�hs@�/@��@�Q�@� �@��@��y@��!@���@�~�@���@��-@��7@�G�@�/@��@��@�Z@�1@�ƨ@��w@���@�+@��@�~�@��@��@��j@�9X@���@�|�@��@�~�@�$�@���@�x�@�V@��@�Z@�1'@��@���@��
@��w@���@���@���@��@�t�@�l�@�\)@�K�@��y@��+@�ff@�E�@�5?@��@�@���@��#@��h@�x�@�p�@�X@�G�@�/@��@�Ĝ@��9@���@��D@�r�@�A�@�1'@�b@��@
=@~�@~��@~E�@}�-@}�@}`B@|�/@{�F@{dZ@y��@x��@x��@xĜ@x�9@xr�@w�w@wK�@vȴ@v5?@u�@u��@u�h@up�@u?}@t��@tZ@t1@s�F@sdZ@s33@s@r�\@rM�@r-@r�@q��@q��@q��@q��@q��@q�7@q7L@p�`@pbN@p �@o�@o;d@n�@n{@m/@m�@l��@k�
@kC�@k"�@ko@ko@k@j�@j��@j��@jM�@i��@i��@i��@i��@i��@iX@iG�@i7L@h��@h�u@g�@gl�@g�@f�R@e�T@e�h@e`B@e`B@eV@d��@dZ@d1@c�
@c��@cC�@c33@b�H@b��@b��@b~�@b=q@b�@a��@a��@a�^@aX@a�@`Ĝ@`��@`�u@`bN@`1'@_\)@_�@^ȴ@^v�@^$�@]�@]`B@]/@\�@\�D@[�
@[��@[��@[�@Z��@Y��@Y��@Y��@YX@X�9@XbN@W��@W+@W
=@V�y@V��@V��@V�+@Vv�@Vff@Vff@Vff@VV@V5?@U�h@Tj@S�
@St�@S"�@R�@R��@R�@Q�^@Qx�@Qhs@Q&�@P��@P�u@PA�@O�@O�w@O�P@O;d@N��@NV@N5?@M��@M��@Mp�@L�@L�@L��@Lz�@K��@K�@J��@JJ@IX@HĜ@HQ�@H1'@HA�@HA�@H1'@H �@Hb@Hb@H  @G�@G�@G�;@G��@G��@G�w@G�w@G�w@G��@G�P@GK�@F�@F�R@F�R@Fv�@E@EO�@E/@D�j@D��@D(�@C��@C�
@C�F@C��@C�@CdZ@C33@C33@B�H@B�\@BJ@Ax�@AX@AG�@A&�@@��@@�@@1'@?�P@?�@>ȴ@>��@>5?@=�T@=��@=�@=/@=V@<��@<Z@;��@;��@;��@;�
@;t�@;o@:�H@:�\@:�@9�^@9hs@9�@8bN@7��@7��@7;d@7
=@6�@6V@6{@6{@6{@5�T@5p�@4�@4Z@49X@4(�@3�m@3�F@3��@3��@3��@3��@3�@333@2~�@2M�@2=q@1��@1��@1G�@0��@01'@0b@/�@/�@/�;@/�w@/�@/K�@.��@-�T@-p�@-O�@-?}@-?}@-/@-/@-�@-V@,�/@,�@,z�@,j@,j@,9X@,1@+��@+ƨ@+�F@+��@+��@+t�@+@*��@*-@)��@)��@)x�@)G�@(��@(bN@'�@'|�@'K�@'
=@&�R@&ff@&V@&5?@%�-@%O�@$�@$�j@$z�@$I�@$9X@$�@#��@#dZ@#"�@"��@"~�@"n�@"n�@"^5@"=q@"J@!�@!��@!7L@!%@!%@ ��@ ��@ Ĝ@ �9@ ��@ �u@ �u@ �u@ �u@ �u@ �@ bN@ 1'@  �@ b@�@�;@\)@;d@�@��@�y@ȴ@��@�+@5?@@`B@V@�/@�j@�@�D@�@�m@��@S�@�@�!@=q@��@��@G�@��@Q�@�@��@|�@;d@�@�R@��@v�@E�@$�@@�T@��@O�@/@��@�@j@9X@1@�m@�F@��@��@t�@S�@C�@C�@"�@o@�H@n�@�@��@x�@�@��@��@��@��@�9@�9@��@�u@bN@1'@ �@��@�P@\)@��@�R@��@V@E�@5?@@�T@@�-@p�@`B@?}@�/@j@Z@I�@(�@1@�m@�
@��@��@��@��@dZ@"�@@
��@
-@	�^@	��@	x�@	hs@	hs@	&�@�`@�9@�u@�@bN@1'@�@�@�P@|�@\)@K�@K�@+@��@�y@�@�@ȴ@��@v�@5?@@@�@O�@�@��@�/@�j@��@z�@j@Z@I�@9X@�@�@��@�m@��@33@o@�@�@�@@�H@�H@��@~�@^5@-@��@�#@�#@�#@�#@�#@��@�^@��@�7@x�@&�@ �9@ r�@ Q�@ A�?���?�\)?��?���?���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�O�A�M�A�M�A�M�A�I�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�M�A�G�A˃A�t�A�(�A�&�A�A��A��yA�1'A���A��A�bNA�hsA��A�1A�r�A�ȴA�=qA�$�A�\)A���A��A�\)A��-A�/A��PA��TA���A�S�A��
A�A���A�jA� �A�A�+A�ffA��^A�A�oA��^A�33A���A��#A���A��A�|�A��FA��A�^5A�dZA�A�5?A��uA��HA�5?A���A���A�VA��A�33A���A���A��A�K�A��A��A�=qA���A�;dA�|�A��A��9A�hsA���A�n�A�jAoA}S�AzbAw�Av��AuhsAtZAs��Aq��Ap��Ap^5Ao�Ao&�An�\AmƨAlffAk?}AjQ�Ai�hAh�Ah��AhVAh�Ag�7AgVAf��Ae��Ad9XAc7LAb��AaƨAa�A`�uA`Q�A_��A]�A\n�AZ�DAX�DAWoAV�AV=qAT~�ARr�AQhsAO�hAN��AM�AL��AKG�AJ=qAI��AH�RAF��AE��AD�AC�AC�wACdZAA�hA@��A@A>�yA>��A>9XA=XA<�A<=qA;�A:�A7�A6 �A5��A4ĜA4Q�A3��A3&�A3
=A2Q�A1+A0��A/A-�TA,bNA+VA)�A(�DA'��A&9XA%G�A$�\A"�A!|�A �RA\)AA7LA��A�A7LAI�At�A��Az�AA�A^5AAdZA�TA�\A�#AO�A�A��A��A�;AXA
Q�A	�-A	�A��AA�A�FA�AjA�A��AoA^5A��A��A��A�A ��A 5?A b@��
@���@���@��-@�b@��#@���@�Q�@��@��@�"�@���@�P@�$�@��@��@�"�@�E�@�{@��^@�h@�x�@��@�u@���@���@��@�z�@��@�|�@�X@�5?@�^@ᙚ@���@��m@��@���@��`@��@��T@�p�@ؼj@׍P@�33@�@�@�V@�@�&�@ϥ�@ΰ!@͑h@���@��@�M�@ȣ�@��y@�`B@���@�ƨ@�t�@�33@¸R@��h@��j@�dZ@���@���@��`@�ƨ@�$�@�bN@�o@���@�~�@���@�&�@��`@�9X@���@�;d@���@��7@��@���@�A�@��
@���@�;d@��H@��\@���@�S�@�x�@���@�r�@��@��@��P@��@�x�@�A�@��w@���@�"�@��@�bN@�l�@���@�-@�&�@��j@�A�@��m@�t�@�dZ@�S�@�@��!@��\@�n�@�^5@�^5@��@���@��@� �@��@���@�^5@��#@�`B@���@��@�Z@��@�|�@���@�V@��@���@�hs@�/@��@�Q�@� �@��@��y@��!@���@�~�@���@��-@��7@�G�@�/@��@��@�Z@�1@�ƨ@��w@���@�+@��@�~�@��@��@��j@�9X@���@�|�@��@�~�@�$�@���@�x�@�V@��@�Z@�1'@��@���@��
@��w@���@���@���@��@�t�@�l�@�\)@�K�@��y@��+@�ff@�E�@�5?@��@�@���@��#@��h@�x�@�p�@�X@�G�@�/@��@�Ĝ@��9@���@��D@�r�@�A�@�1'@�b@��@
=@~�@~��@~E�@}�-@}�@}`B@|�/@{�F@{dZ@y��@x��@x��@xĜ@x�9@xr�@w�w@wK�@vȴ@v5?@u�@u��@u�h@up�@u?}@t��@tZ@t1@s�F@sdZ@s33@s@r�\@rM�@r-@r�@q��@q��@q��@q��@q��@q�7@q7L@p�`@pbN@p �@o�@o;d@n�@n{@m/@m�@l��@k�
@kC�@k"�@ko@ko@k@j�@j��@j��@jM�@i��@i��@i��@i��@i��@iX@iG�@i7L@h��@h�u@g�@gl�@g�@f�R@e�T@e�h@e`B@e`B@eV@d��@dZ@d1@c�
@c��@cC�@c33@b�H@b��@b��@b~�@b=q@b�@a��@a��@a�^@aX@a�@`Ĝ@`��@`�u@`bN@`1'@_\)@_�@^ȴ@^v�@^$�@]�@]`B@]/@\�@\�D@[�
@[��@[��@[�@Z��@Y��@Y��@Y��@YX@X�9@XbN@W��@W+@W
=@V�y@V��@V��@V�+@Vv�@Vff@Vff@Vff@VV@V5?@U�h@Tj@S�
@St�@S"�@R�@R��@R�@Q�^@Qx�@Qhs@Q&�@P��@P�u@PA�@O�@O�w@O�P@O;d@N��@NV@N5?@M��@M��@Mp�@L�@L�@L��@Lz�@K��@K�@J��@JJ@IX@HĜ@HQ�@H1'@HA�@HA�@H1'@H �@Hb@Hb@H  @G�@G�@G�;@G��@G��@G�w@G�w@G�w@G��@G�P@GK�@F�@F�R@F�R@Fv�@E@EO�@E/@D�j@D��@D(�@C��@C�
@C�F@C��@C�@CdZ@C33@C33@B�H@B�\@BJ@Ax�@AX@AG�@A&�@@��@@�@@1'@?�P@?�@>ȴ@>��@>5?@=�T@=��@=�@=/@=V@<��@<Z@;��@;��@;��@;�
@;t�@;o@:�H@:�\@:�@9�^@9hs@9�@8bN@7��@7��@7;d@7
=@6�@6V@6{@6{@6{@5�T@5p�@4�@4Z@49X@4(�@3�m@3�F@3��@3��@3��@3��@3�@333@2~�@2M�@2=q@1��@1��@1G�@0��@01'@0b@/�@/�@/�;@/�w@/�@/K�@.��@-�T@-p�@-O�@-?}@-?}@-/@-/@-�@-V@,�/@,�@,z�@,j@,j@,9X@,1@+��@+ƨ@+�F@+��@+��@+t�@+@*��@*-@)��@)��@)x�@)G�@(��@(bN@'�@'|�@'K�@'
=@&�R@&ff@&V@&5?@%�-@%O�@$�@$�j@$z�@$I�@$9X@$�@#��@#dZ@#"�@"��@"~�@"n�@"n�@"^5@"=q@"J@!�@!��@!7L@!%@!%@ ��@ ��@ Ĝ@ �9@ ��@ �u@ �u@ �u@ �u@ �u@ �@ bN@ 1'@  �@ b@�@�;@\)@;d@�@��@�y@ȴ@��@�+@5?@@`B@V@�/@�j@�@�D@�@�m@��@S�@�@�!@=q@��@��@G�@��@Q�@�@��@|�@;d@�@�R@��@v�@E�@$�@@�T@��@O�@/@��@�@j@9X@1@�m@�F@��@��@t�@S�@C�@C�@"�@o@�H@n�@�@��@x�@�@��@��@��@��@�9@�9@��@�u@bN@1'@ �@��@�P@\)@��@�R@��@V@E�@5?@@�T@@�-@p�@`B@?}@�/@j@Z@I�@(�@1@�m@�
@��@��@��@��@dZ@"�@@
��@
-@	�^@	��@	x�@	hs@	hs@	&�@�`@�9@�u@�@bN@1'@�@�@�P@|�@\)@K�@K�@+@��@�y@�@�@ȴ@��@v�@5?@@@�@O�@�@��@�/@�j@��@z�@j@Z@I�@9X@�@�@��@�m@��@33@o@�@�@�@@�H@�H@��@~�@^5@-@��@�#@�#@�#@�#@�#@��@�^@��@�7@x�@&�@ �9@ r�@ Q�@ A�?���?�\)?��?���?���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BŢBŢBƨBǮBǮBǮBƨBƨBƨBƨBǮBǮBȴBȴBŢB�qB��B��B&�BA�BF�B+B�B�RB�BB�#B��B�
B�wB�7B��B�DBx�Bw�Bv�Bv�Bo�Bs�By�Bu�Bn�Bl�BhsBaHBR�BaHBS�B33B �B.B!�B{BB�TB�B�B�/BǮB��B��B�hB��B��B��B�\B�B�Bs�BjBcTBdZB]/BQ�B;dB#�B)�B�B%B
��B
�fB
��B
�B
��B
ƨB
�FB
�RB
�B
��B
�hB
w�B
iyB
e`B
L�B
;dB
'�B
33B
(�B
�B
 �B
JB

=B
oB
PB
  B	��B	�B	�TB	�HB	�5B	�5B	�)B	�BB	�B	�
B	��B	ȴB	ƨB	�3B	��B	�B	��B	��B	��B	��B	��B	�PB	m�B	p�B	XB	T�B	S�B	`BB	W
B	>wB	.B	7LB	(�B	.B	,B	�B	�B	�B	�B	oB	  B		7B		7B	+B	DB	B�B�B�B�fB�B�B�fB�sB�;B�BȴB�B�!B��B�wBĜBBBB�FB��B�B��B�bB�PB�DB�1B~�B�+Bs�Bs�Bu�BiyBbNBm�BbNB_;Bk�Bn�BiyBe`B`BB[#BM�BK�BS�BXBQ�BS�BK�B;dB9XBD�BD�B@�B;dB8RB:^B>wB6FB:^BB�B9XB5?B5?B/B49B1'B33B+B#�B!�B�B�B �B'�B+B0!B.B+B#�B�B�B{B!�B(�B(�B$�B�B{BJB�B�B"�B�B"�B,B+B+B)�B%�B�B�B�B#�B!�B�B�BVBB�B!�B�B�B�B�B�BuB�B$�B&�B#�B,B-B%�B�B�B�B�B&�B$�B'�B$�B�B�B�B#�B(�B7LB6FB49B0!B(�B,B,B1'B0!B.B-B,B.B6FBD�BA�B>wBA�BD�B@�BC�BE�BE�B@�BI�BM�BM�BM�BO�BN�BO�BM�BF�B>wBF�B]/B]/B`BBcTBcTB]/BXB`BBk�Bn�Bl�BffBe`Br�Bx�B� B}�B�B�1B�JB�VB�hB�hB�bB�hB�uB�{B�{B�{B�oB�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�RB�RB�LB�wB�}B�}B�dBȴB��B��BȴB��B��B��B��B�B��B��B�)B�;B�TB�NB�HB�fB�`B�mB�B�B�B��B��B��B	  B	B	B	%B	1B	JB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	)�B	+B	,B	,B	,B	.B	/B	1'B	5?B	8RB	7LB	7LB	7LB	7LB	:^B	=qB	>wB	?}B	@�B	@�B	C�B	D�B	F�B	J�B	O�B	P�B	Q�B	R�B	W
B	XB	W
B	W
B	^5B	\)B	iyB	o�B	p�B	o�B	n�B	n�B	r�B	s�B	w�B	z�B	|�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�B	�%B	�1B	�+B	�7B	�JB	�PB	�PB	�PB	�\B	�bB	�bB	�bB	�\B	�bB	�bB	�oB	�uB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�'B	�3B	�9B	�9B	�3B	�3B	�9B	�LB	�RB	�RB	�XB	��B	��B	ÖB	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�/B	�BB	�BB	�BB	�BB	�ZB	�`B	�ZB	�NB	�HB	�B	�B	�sB	�mB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
PB
VB
PB
PB
PB
PB
VB
PB
PB
VB
VB
VB
VB
VB
VB
VB
PB
PB
PB
JB
VB
VB
PB
JB
\B
hB
bB
oB
oB
uB
�B
{B
�B
�B
�B
{B
�B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
�B
�B
!�B
!�B
 �B
!�B
!�B
"�B
!�B
#�B
&�B
&�B
'�B
'�B
&�B
)�B
+B
)�B
(�B
'�B
(�B
)�B
-B
-B
-B
.B
/B
/B
/B
.B
.B
-B
+B
.B
/B
.B
.B
.B
.B
0!B
33B
33B
33B
33B
33B
2-B
1'B
0!B
1'B
49B
6FB
7LB
7LB
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
8RB
9XB
9XB
8RB
7LB
6FB
7LB
7LB
9XB
:^B
9XB
9XB
8RB
9XB
8RB
<jB
=qB
=qB
=qB
=qB
?}B
>wB
<jB
>wB
>wB
@�B
A�B
A�B
B�B
A�B
A�B
?}B
A�B
A�B
C�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
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
H�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
G�B
I�B
J�B
K�B
L�B
L�B
K�B
J�B
L�B
L�B
L�B
L�B
M�B
L�B
N�B
N�B
N�B
N�B
N�B
P�B
R�B
Q�B
R�B
R�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
VB
VB
W
B
VB
W
B
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
YB
[#B
[#B
[#B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
aHB
`BB
bNB
bNB
bNB
aHB
bNB
bNB
bNB
cTB
bNB
bNB
bNB
dZB
e`B
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
e`B
e`B
ffB
e`B
e`B
ffB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
iyB
iyB
iyB
jB
jB
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
s�B
r�B
r�B
t�B
u�B
u�B
t�B
v�B
w�B
w�B
x�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BŢBŢBƨBǮBǮBǮBƨBƨBƨBƨBǮBǮBȴB��B�?B�}BרB��B)�BFYBN<B�B�OB��B��B��B�$B�_B��B�B�B�B|�By�BxlBxBq[Bu%B{Bw2Bo�BmCBiDBb�BT�Ba�BU�B6FB#�B/iB#�BSB_B�B��B��B��B�#B��B��B�{B�4B�yB��B��B��B�GBuZBl"Bd�Bd�B^BSB=�B&�B+QB�BKB
��B
�*B
�B
�B
�2B
�B
�B
�>B
�B
�B
��B
z�B
lB
g�B
OvB
>�B
*�B
49B
*�B
!-B
!�B
�B
DB
�B
�B
B	��B	��B	�B	�B	�VB	�!B	��B	�vB	ڠB	�sB	ѝB	�lB	�EB	��B	��B	�"B	��B	��B	��B	�jB	�#B	�VB	pB	r�B	Z�B	WYB	U�B	`�B	XB	@�B	0�B	8�B	+B	/5B	-)B	VB	_B	�B	�B	�B	uB	
�B	
rB	KB	�B	�B�B�B��B�B�'B�;B�B�B�\B�?B�rB�aB�GB�[B��B�9B�{B�-B��B��B��B� B��B� B�\B�B��B��B�Bv+Bu?BwBk�BdZBn�Bd@Ba-BlWBoiBjBf�Ba�B\xBO�BM�BT�BX�BSBT�BMB=qB;0BE�BE�BA�B<�B9�B;B?cB7�B;JBB�B:^B6+B6B0UB4�B1�B3�B+�B$�B"�B �B�B!�B(�B+�B0oB.cB+kB$�B�B�B�B"hB)_B)DB%,B]B�B�BeBdB#nB�B#nB,"B+kB+6B*0B&LB vB�BjB$tB"NB BB]B�BB�B"B]BQBkB_BqB�BdB%`B'�B$�B,qB-wB&�B�B#BB�B'�B%�B(�B%�BB�B�B$�B)�B7fB6�B4�B0�B)�B,�B,�B1�B0�B.�B.B-]B/OB7BD�BBB?HBBBD�BABDBFBF%BAoBJ=BN<BN<BN<BP.BOBBPHBN<BG�B@4BG�B]dB]�B`�Bc�Bc�B]�BY1Ba-Bk�Bn�Bm)BgRBf�BsMBy�B��B~�B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B��B��B�B�B�#B�B�HB�@B�2B�8B�>B�mB�yB��B��B��B��B��B��B��B��B��B�6B��B��B��B�7B�B� B� B�B�B�2B�{B�xBߊB�nB�B�B�B��B��B��B�B�B�B�$B�^B	 OB	UB	gB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	;B	$&B	*B	+B	,=B	,=B	,"B	./B	/OB	1vB	5ZB	8�B	7fB	7fB	7fB	7�B	:xB	=�B	>�B	?�B	@�B	@�B	C�B	D�B	F�B	J�B	O�B	Q B	R B	S&B	W$B	X+B	WYB	WsB	^jB	\�B	i�B	o�B	p�B	o�B	n�B	n�B	r�B	s�B	xB	{B	}B	}B	~(B	~(B	~(B	.B	�'B	�GB	�MB	�?B	�KB	�_B	�RB	�dB	�jB	�jB	�jB	�vB	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�*B	�B	�B	�B	�/B	�/B	�)B	�)B	�CB	�5B	�;B	�-B	�GB	�AB	�MB	�TB	�TB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	�B	��B	��B	� B	�B	�B	�B	�&B	�&B	�B	�B	�B	�,B	�,B	�B	�$B	�+B	�1B	�EB	�+B	�SB	�=B	�=B	�CB	�]B	�OB	�dB	�\B	�vB	�vB	�B	�tB	�zB	�tB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�"B	�"B
 4B
 B
 4B
 B
'B
;B
-B
3B
-B
;B
;B
UB
uB
mB
_B

XB
jB
VB
PB
PB
PB
jB
VB
�B
jB
VB
pB
VB
VB
VB
VB
VB
jB
jB
jB
~B
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
�B
�B
!�B
!�B
 �B
!�B
!�B
#B
"B
$B
'B
'B
(
B
(
B
'B
*B
+B
*B
)B
($B
)*B
*0B
-)B
-)B
-CB
./B
/5B
/B
/5B
.IB
./B
-CB
+QB
./B
/B
./B
.IB
.IB
.IB
0UB
33B
3MB
3MB
3MB
3hB
2GB
1[B
0oB
1vB
4TB
6`B
7LB
7LB
7LB
7LB
7fB
7fB
7fB
7fB
7fB
8RB
8RB
8lB
8lB
8lB
8lB
9XB
9rB
8RB
7fB
6zB
7fB
7�B
9XB
:xB
9�B
9rB
8�B
9�B
8�B
<�B
=�B
=�B
=�B
=�B
?�B
>�B
<�B
>�B
>�B
@�B
A�B
A�B
B�B
A�B
A�B
?�B
A�B
A�B
C�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
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
H�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
G�B
I�B
J�B
K�B
L�B
L�B
K�B
KB
L�B
L�B
L�B
MB
M�B
MB
N�B
N�B
OB
O(B
O(B
Q B
SB
RB
SB
S&B
UB
UB
UB
UB
VB
VB
V9B
UB
VB
VB
W$B
VB
W$B
X+B
X+B
YKB
Y1B
ZB
Z7B
Z7B
Z7B
ZB
ZB
ZQB
Z7B
YKB
XEB
YKB
[WB
[=B
[=B
^5B
^5B
_;B
^OB
^OB
_;B
_;B
^OB
^OB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
abB
`vB
bNB
bhB
bhB
abB
bhB
bhB
bhB
cnB
b�B
b�B
b�B
dZB
e`B
d�B
ezB
ezB
e`B
ezB
ffB
f�B
ffB
ezB
ezB
f�B
e�B
e�B
f�B
h�B
h�B
hsB
hsB
h�B
h�B
i�B
i�B
jB
i�B
i�B
i�B
j�B
j�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
s�B
r�B
r�B
t�B
u�B
u�B
t�B
v�B
w�B
w�B
x�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<jJ�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807140036022018071400360220180714003602201807140200162018071402001620180714020016201807150026502018071500265020180715002650  JA  ARFMdecpA19c                                                                20180723123519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033623  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033623  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033624  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033624  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033624  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033624  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723033624                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040121                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180710153311  CV  JULD            G�O�G�O�FÅ�                JM  ARCAJMQC2.0                                                                 20180713153602  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180713153602  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180713170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180714152650  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                