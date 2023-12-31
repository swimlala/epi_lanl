CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-18T00:35:21Z creation;2018-03-18T00:35:25Z conversion to V3.1;2019-12-19T07:42:54Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180318003521  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_221                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�T4��t 1   @�T5����@4m�c�A �dA�.H�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C${C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CJ{CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC�
=C�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\xRD\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��DwxRDw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�S�A�=qA��A��A�
=A��
AƑhA�  A�Q�A� �A���A���A�AĸRAđhA�bNA�=qA��A���Aã�A�z�A�A�A�1A���A©�A�l�A�A�A��A��`A�ƨA�ZA��A��uA�?}A�bNA�x�A�9XA��A�A��jA�x�A�n�A�l�A�Q�A���A��`A�9XA�E�A���A�E�A��A��PA��
A��DA�\)A���A�A��A�"�A�+A��A��A���A��yA��A��A�bNA�jA��A���A�;dA�?}A�ȴA�VA�A��-A�=qA� �A��A��wA�A�=qA��HA�M�A��FA�l�A�A�A���A� �A���A��+A�jA�/A���A���A��A���A���A��A��wA��FA�-A�A���A��+A��FA��-A�"�Al�A~�A|�Ay�FAx$�Av��Au/Asl�ArbNAp�DAo%Am�Al�/Ak�Aj�DAi�Ag�wAf  Ac�;A_A^(�A\�yA[+AYK�AXM�AW?}AU�-ARn�AOl�AN��AL��AJ�yAH�AF�\AB��AAG�A@��A?�A>z�A=/A:��A:1'A9K�A8�/A8z�A81A7t�A77LA6��A6n�A5�A5�A2  A/�A/&�A.�A-ƨA,1'A*�A*ffA*ffA*M�A*5?A)��A(��A&ȴA$ȴA$�A#G�A"��A"�A"1'A!|�A �9A�A  AA�A�An�A�TA^5AJAAz�A��A33A�\A\)AbAz�AdZA
��A	�PA�An�A�#A�wAp�An�A|�AȴA1AoA�#A ��@���@�"�@���@�=q@���@��@��`@��@��D@�Z@�~�@�=q@�@��@�ff@�X@�@���@��H@��@�j@� �@�\@��@���@��;@◍@�%@�9X@�+@ޏ\@ܼj@�1'@ۍP@�\)@���@���@�7L@��@��@�I�@׍P@ְ!@�=q@թ�@�(�@��T@�bN@ϥ�@��H@Ο�@·+@���@͙�@�p�@�bN@˥�@�@�v�@�p�@��;@�l�@�C�@�-@��@�l�@��^@�V@�A�@��@�o@���@���@�Z@�|�@��!@�M�@��@���@��u@�z�@�I�@�K�@��H@�V@�@�?}@�I�@���@�
=@�E�@��-@�V@�I�@��F@��H@��\@�$�@�@�?}@�V@��D@���@��@���@�dZ@�K�@�"�@�@�ȴ@�=q@�X@���@�z�@�(�@��@�C�@�C�@��@�ȴ@�v�@�=q@�$�@��@��^@��h@�O�@�?}@�&�@�V@��`@�j@�1'@��
@��F@��@�S�@�;d@�33@�o@�@��@��@�"�@�
=@���@�~�@���@��@�
=@��y@��y@��@�ȴ@�~�@�v�@�-@�{@���@���@��@�hs@�/@���@��u@��@��@��w@��P@�@�@��H@�o@�o@��y@�M�@���@��9@��@��@�j@�z�@�Z@���@��
@��;@��
@���@��F@��P@�S�@�@���@��+@�5?@��^@�7L@��9@�r�@�9X@��@�1@��@�ƨ@��F@���@��P@��@�C�@��H@��!@�v�@�E�@��@��^@��7@�p�@��@�j@�(�@�(�@� �@� �@� �@��;@��P@�C�@���@���@�~�@�ff@�-@���@�X@��@��`@�Ĝ@���@�1@��w@��w@��F@���@��P@�t�@�;d@�o@��y@��\@�M�@�M�@�{@���@�x�@�%@�j@��@��
@�ƨ@��@���@��@�33@��H@���@�v�@�v�@�~�@�~�@�M�@�{@��#@���@�O�@��`@�j@�1'@���@�l�@���@��\@�n�@�E�@�=q@�-@���@��-@��@�&�@��j@���@��@�I�@��@�b@��@��w@���@�t�@�;d@�@�ȴ@��!@���@�^5@�@���@���@�&�@��/@��9@�j@�A�@�(�@��@�1@�  @�@�w@��@|�@;d@~��@~�@~v�@~$�@~{@}��@}�-@}�@}`B@|�@|Z@|9X@|�@{�m@{��@{dZ@{o@z�H@z�!@z��@z��@y��@y%@xbN@w��@v��@vv�@vV@v@u�@t�/@tj@t�@s33@r�H@r��@r=q@q7L@p�9@p��@pr�@p  @p  @o�@o�;@o��@ol�@n{@m?}@l�j@l��@lZ@k��@k�F@k"�@jn�@i��@i��@i��@iX@i&�@i%@hĜ@h�u@h1'@gK�@g
=@f��@fȴ@f�+@e�T@e�@d��@dI�@d1@c��@c�
@c��@b�!@b~�@b�@a�@ax�@`�u@_|�@_
=@^��@^��@^�y@^�R@^ff@]�@]?}@]V@\�@\�@\9X@[��@[�@[dZ@[S�@[33@["�@[o@Z��@Z~�@Z^5@Z=q@Y��@XA�@W��@W|�@W\)@V��@V{@U�-@U�h@U`B@UO�@UV@T��@T��@T��@UV@UV@T��@T�@T�@T��@T�/@T1@R��@R~�@R^5@R^5@R^5@R�@Q�^@Q7L@PĜ@PA�@O�;@Ol�@O;d@N��@N��@Nv�@NV@NV@NV@NE�@N$�@M�@M�-@Mp�@MV@Lz�@K��@K�@J�@J�!@J^5@JJ@IX@H�`@H�9@HbN@G�@G�P@Gl�@Gl�@Gl�@Gl�@F�+@FV@F5?@F$�@F{@F@E�T@E��@E@E@E��@E�h@E`B@E?}@EV@D�/@D��@Dz�@D9X@C�m@CC�@B�@B��@B��@BM�@A�@A�7@@Ĝ@@�@@r�@@ �@?�w@?�@?��@?�P@?K�@>�@>ff@>E�@=�T@=�h@=�@=p�@<��@<z�@<�@;�@;t�@;S�@;33@:��@:n�@:^5@:�@:�@9�@9��@9�^@9��@9X@97L@9&�@9�@8��@8�u@8r�@81'@7��@7l�@7�@6�+@6E�@5�@5�@5@5�-@5�@5O�@5�@4Z@3�@3dZ@333@3o@2~�@2^5@2M�@2-@1��@1�7@0�`@0��@0b@/�P@/;d@.@-�h@,�@,Z@,(�@,1@+��@+�m@+�F@+t�@*�H@*M�@*-@*-@*-@*-@*J@)�@)�^@)G�@(�`@(��@(r�@(bN@(bN@(A�@(b@(  @'�@'��@'�@'��@'�P@'K�@&ȴ@&��@&5?@%�-@%�@$9X@#dZ@#"�@#"�@#o@#@"�H@"��@"��@"=q@"J@!�@!��@!�7@!x�@!X@!G�@!7L@!&�@!&�@!&�@!&�@!�@!%@ �`@ Ĝ@ Ĝ@ �9@ �@ �@ �@ �@ �@ r�@ r�@ r�@ bN@ Q�@ 1'@   @�w@K�@K�@��@�y@$�@��@�-@p�@/@��@�@z�@�@�m@�F@t�@dZ@C�@"�@o@�@��@�\@^5@^5@^5@M�@M�@=q@=q@=q@=q@-@J@�^@7L@%@�9@��@�@A�@  @��@�P@|�@K�@+@
=@�y@��@ff@5?@@@p�@�@��@z�@Z@I�@(�@1@�m@�
@�
@�
@�
@�
@�m@�m@�
@ƨ@��@t�@o@�@��@^5@M�@J@��@��@��@hs@X@7L@%@��@�9@�u@�u@r�@1'@b@  @�@�;@�@�P@|�@|�@l�@\)@K�@
=@��@��@�+@�+@v�@V@V@5?@5?@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�S�A�=qA��A��A�
=A��
AƑhA�  A�Q�A� �A���A���A�AĸRAđhA�bNA�=qA��A���Aã�A�z�A�A�A�1A���A©�A�l�A�A�A��A��`A�ƨA�ZA��A��uA�?}A�bNA�x�A�9XA��A�A��jA�x�A�n�A�l�A�Q�A���A��`A�9XA�E�A���A�E�A��A��PA��
A��DA�\)A���A�A��A�"�A�+A��A��A���A��yA��A��A�bNA�jA��A���A�;dA�?}A�ȴA�VA�A��-A�=qA� �A��A��wA�A�=qA��HA�M�A��FA�l�A�A�A���A� �A���A��+A�jA�/A���A���A��A���A���A��A��wA��FA�-A�A���A��+A��FA��-A�"�Al�A~�A|�Ay�FAx$�Av��Au/Asl�ArbNAp�DAo%Am�Al�/Ak�Aj�DAi�Ag�wAf  Ac�;A_A^(�A\�yA[+AYK�AXM�AW?}AU�-ARn�AOl�AN��AL��AJ�yAH�AF�\AB��AAG�A@��A?�A>z�A=/A:��A:1'A9K�A8�/A8z�A81A7t�A77LA6��A6n�A5�A5�A2  A/�A/&�A.�A-ƨA,1'A*�A*ffA*ffA*M�A*5?A)��A(��A&ȴA$ȴA$�A#G�A"��A"�A"1'A!|�A �9A�A  AA�A�An�A�TA^5AJAAz�A��A33A�\A\)AbAz�AdZA
��A	�PA�An�A�#A�wAp�An�A|�AȴA1AoA�#A ��@���@�"�@���@�=q@���@��@��`@��@��D@�Z@�~�@�=q@�@��@�ff@�X@�@���@��H@��@�j@� �@�\@��@���@��;@◍@�%@�9X@�+@ޏ\@ܼj@�1'@ۍP@�\)@���@���@�7L@��@��@�I�@׍P@ְ!@�=q@թ�@�(�@��T@�bN@ϥ�@��H@Ο�@·+@���@͙�@�p�@�bN@˥�@�@�v�@�p�@��;@�l�@�C�@�-@��@�l�@��^@�V@�A�@��@�o@���@���@�Z@�|�@��!@�M�@��@���@��u@�z�@�I�@�K�@��H@�V@�@�?}@�I�@���@�
=@�E�@��-@�V@�I�@��F@��H@��\@�$�@�@�?}@�V@��D@���@��@���@�dZ@�K�@�"�@�@�ȴ@�=q@�X@���@�z�@�(�@��@�C�@�C�@��@�ȴ@�v�@�=q@�$�@��@��^@��h@�O�@�?}@�&�@�V@��`@�j@�1'@��
@��F@��@�S�@�;d@�33@�o@�@��@��@�"�@�
=@���@�~�@���@��@�
=@��y@��y@��@�ȴ@�~�@�v�@�-@�{@���@���@��@�hs@�/@���@��u@��@��@��w@��P@�@�@��H@�o@�o@��y@�M�@���@��9@��@��@�j@�z�@�Z@���@��
@��;@��
@���@��F@��P@�S�@�@���@��+@�5?@��^@�7L@��9@�r�@�9X@��@�1@��@�ƨ@��F@���@��P@��@�C�@��H@��!@�v�@�E�@��@��^@��7@�p�@��@�j@�(�@�(�@� �@� �@� �@��;@��P@�C�@���@���@�~�@�ff@�-@���@�X@��@��`@�Ĝ@���@�1@��w@��w@��F@���@��P@�t�@�;d@�o@��y@��\@�M�@�M�@�{@���@�x�@�%@�j@��@��
@�ƨ@��@���@��@�33@��H@���@�v�@�v�@�~�@�~�@�M�@�{@��#@���@�O�@��`@�j@�1'@���@�l�@���@��\@�n�@�E�@�=q@�-@���@��-@��@�&�@��j@���@��@�I�@��@�b@��@��w@���@�t�@�;d@�@�ȴ@��!@���@�^5@�@���@���@�&�@��/@��9@�j@�A�@�(�@��@�1@�  @�@�w@��@|�@;d@~��@~�@~v�@~$�@~{@}��@}�-@}�@}`B@|�@|Z@|9X@|�@{�m@{��@{dZ@{o@z�H@z�!@z��@z��@y��@y%@xbN@w��@v��@vv�@vV@v@u�@t�/@tj@t�@s33@r�H@r��@r=q@q7L@p�9@p��@pr�@p  @p  @o�@o�;@o��@ol�@n{@m?}@l�j@l��@lZ@k��@k�F@k"�@jn�@i��@i��@i��@iX@i&�@i%@hĜ@h�u@h1'@gK�@g
=@f��@fȴ@f�+@e�T@e�@d��@dI�@d1@c��@c�
@c��@b�!@b~�@b�@a�@ax�@`�u@_|�@_
=@^��@^��@^�y@^�R@^ff@]�@]?}@]V@\�@\�@\9X@[��@[�@[dZ@[S�@[33@["�@[o@Z��@Z~�@Z^5@Z=q@Y��@XA�@W��@W|�@W\)@V��@V{@U�-@U�h@U`B@UO�@UV@T��@T��@T��@UV@UV@T��@T�@T�@T��@T�/@T1@R��@R~�@R^5@R^5@R^5@R�@Q�^@Q7L@PĜ@PA�@O�;@Ol�@O;d@N��@N��@Nv�@NV@NV@NV@NE�@N$�@M�@M�-@Mp�@MV@Lz�@K��@K�@J�@J�!@J^5@JJ@IX@H�`@H�9@HbN@G�@G�P@Gl�@Gl�@Gl�@Gl�@F�+@FV@F5?@F$�@F{@F@E�T@E��@E@E@E��@E�h@E`B@E?}@EV@D�/@D��@Dz�@D9X@C�m@CC�@B�@B��@B��@BM�@A�@A�7@@Ĝ@@�@@r�@@ �@?�w@?�@?��@?�P@?K�@>�@>ff@>E�@=�T@=�h@=�@=p�@<��@<z�@<�@;�@;t�@;S�@;33@:��@:n�@:^5@:�@:�@9�@9��@9�^@9��@9X@97L@9&�@9�@8��@8�u@8r�@81'@7��@7l�@7�@6�+@6E�@5�@5�@5@5�-@5�@5O�@5�@4Z@3�@3dZ@333@3o@2~�@2^5@2M�@2-@1��@1�7@0�`@0��@0b@/�P@/;d@.@-�h@,�@,Z@,(�@,1@+��@+�m@+�F@+t�@*�H@*M�@*-@*-@*-@*-@*J@)�@)�^@)G�@(�`@(��@(r�@(bN@(bN@(A�@(b@(  @'�@'��@'�@'��@'�P@'K�@&ȴ@&��@&5?@%�-@%�@$9X@#dZ@#"�@#"�@#o@#@"�H@"��@"��@"=q@"J@!�@!��@!�7@!x�@!X@!G�@!7L@!&�@!&�@!&�@!&�@!�@!%@ �`@ Ĝ@ Ĝ@ �9@ �@ �@ �@ �@ �@ r�@ r�@ r�@ bN@ Q�@ 1'@   @�w@K�@K�@��@�y@$�@��@�-@p�@/@��@�@z�@�@�m@�F@t�@dZ@C�@"�@o@�@��@�\@^5@^5@^5@M�@M�@=q@=q@=q@=q@-@J@�^@7L@%@�9@��@�@A�@  @��@�P@|�@K�@+@
=@�y@��@ff@5?@@@p�@�@��@z�@Z@I�@(�@1@�m@�
@�
@�
@�
@�
@�m@�m@�
@ƨ@��@t�@o@�@��@^5@M�@J@��@��@��@hs@X@7L@%@��@�9@�u@�u@r�@1'@b@  @�@�;@�@�P@|�@|�@l�@\)@K�@
=@��@��@�+@�+@v�@V@V@5?@5?@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B.B-B,B.B+B&�B#�B�B�B'�B&�B'�B$�B#�B�B�B�B�B#�B$�B#�B%�B'�B)�B)�B)�B+B)�B(�B'�B$�B%�B+B5?BK�BiyB�7B��B��B�'B�LB�^B�}B��BȴBVBL�BR�B^5BaHBiyBk�BiyBv�B|�B~�B�B��B��B�!B��Bz�B_;BbNBq�BiyB^5BP�BJ�B(�BbBVB�BoB+B�BBB�BŢB�'B�LB��B�%B�uB��B��B|�B�7B�B�B� Bv�BhsBR�B/B+B�BB
��B
�}B
�-B
��B
�B
l�B
K�B
-B
=qB
�B
�B
uB	��B	�B	�B	�;B	��B	�B	ƨB	��B	�^B	��B	�LB	�B	��B	��B	�PB	{�B	W
B	q�B	l�B	\)B	O�B	P�B	G�B	2-B	�B	hB	�B	
=B��B�B�/B��B��B�5B�BƨBB�-BŢB��BBB�}B�qB�jB�^B�9B�B��B~�Bv�B��B�{B�bB�B�DB��B��B��B��B�hB�B{�Bt�B�7B�7B�PB�PB�DB�B}�Bv�Bz�B�B}�Br�B_;BH�B33BgmB]/B]/BK�BK�BXBL�BL�BD�BL�BR�BS�BXB_;B[#BbNB\)BQ�BO�BR�BP�BK�BD�BD�BC�BA�B]/B`BB_;B`BBcTBcTBbNB]/BN�B;dBJ�B[#B]/BZBZBZBXB[#BVB^5BZB\)Be`BaHB]/B_;BhsBgmBjBbNBn�Bm�Bp�Bm�Bl�Bn�Bs�Bs�Bl�Bn�Bl�Bp�Bk�BdZBaHBjBv�Bx�B~�B�B}�B~�B� B{�B{�B~�B� B|�B|�B�+B�DB�+B~�B�VB�=B��B��B��B��B��B��B�B�B�B�FB�RB�FB��BƨBƨBÖBɺB��B��B��B��B�
B�#B�/B�ZB�yB�B�B��B��B	  B	B	B	1B	1B	
=B	hB	�B	�B	�B	�B	"�B	$�B	#�B	&�B	1'B	2-B	7LB	9XB	A�B	C�B	I�B	J�B	N�B	O�B	R�B	VB	ZB	\)B	^5B	cTB	e`B	e`B	e`B	ffB	k�B	k�B	m�B	o�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	x�B	x�B	z�B	� B	�%B	�PB	�DB	�JB	�PB	�\B	�VB	�PB	�bB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�FB	�^B	�jB	�qB	�wB	��B	��B	��B	ÖB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�#B	�/B	�5B	�5B	�BB	�BB	�HB	�BB	�HB	�mB	�B	�B	�B	�B	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B	��B	��B
B
B
+B
1B
+B
%B
1B

=B
DB

=B
DB
DB
PB
PB
DB
JB
bB
hB
hB
uB
oB
hB
bB
hB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
#�B
#�B
"�B
!�B
!�B
"�B
#�B
!�B
$�B
%�B
#�B
!�B
$�B
'�B
'�B
&�B
(�B
(�B
'�B
%�B
$�B
!�B
#�B
&�B
(�B
'�B
'�B
'�B
&�B
&�B
(�B
+B
+B
+B
+B
,B
+B
+B
)�B
(�B
,B
-B
,B
,B
)�B
)�B
,B
.B
.B
/B
.B
-B
+B
.B
.B
.B
-B
+B
,B
1'B
2-B
33B
2-B
1'B
1'B
0!B
0!B
2-B
33B
2-B
2-B
33B
33B
5?B
5?B
5?B
5?B
49B
49B
49B
49B
33B
1'B
/B
33B
7LB
6FB
49B
5?B
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
9XB
9XB
9XB
9XB
7LB
49B
49B
:^B
;dB
;dB
;dB
:^B
9XB
9XB
:^B
:^B
;dB
;dB
=qB
<jB
=qB
>wB
>wB
?}B
>wB
>wB
>wB
=qB
=qB
=qB
<jB
;dB
<jB
=qB
=qB
?}B
?}B
>wB
>wB
?}B
A�B
A�B
@�B
D�B
D�B
E�B
E�B
D�B
A�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
E�B
G�B
H�B
H�B
H�B
G�B
G�B
G�B
I�B
K�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
L�B
K�B
L�B
M�B
L�B
K�B
K�B
L�B
L�B
O�B
N�B
N�B
M�B
O�B
P�B
O�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
Q�B
Q�B
VB
VB
VB
T�B
W
B
W
B
W
B
VB
T�B
S�B
VB
T�B
VB
W
B
R�B
XB
XB
YB
\)B
\)B
]/B
\)B
\)B
[#B
[#B
\)B
^5B
_;B
_;B
^5B
^5B
^5B
]/B
]/B
]/B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
^5B
_;B
_;B
^5B
^5B
^5B
_;B
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
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
ffB
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
gmB
gmB
ffB
ffB
ffB
e`B
e`B
gmB
ffB
ffB
e`B
ffB
hsB
hsB
hsB
gmB
iyB
iyB
hsB
iyB
jB
iyB
k�B
jB
jB
k�B
k�B
jB
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
k�B
jB
jB
l�B
l�B
m�B
m�B
l�B
l�B
l�B
n�B
n�B
m�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
x�B
w�B
w�B
x�B
x�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B./B-CB,=B./B+QB'mB$�B�B�B(XB'8B($B%B$B 'BBBIB$&B%FB$@B&fB(sB*eB*eB*B+kB*eB)yB(XB%�B&�B+�B5�BK�BiyB��B��B�8B��B��B�xB�}B��BȴBpBM�BT�B_;BbhBjKBlqBj�BwfB}qB�B��B�\B��B�G�O�B�Bc�BdBrGBj�B`BS@BMPB-�B�B�BeB,B
rB�3B%BG�O�BɠB��B��B��B��B��B�B�KB�OB�#B��B��B�iBw�Bi�BV9B3MB/OBKB�B
��B
�B
��B
��B
�9B
pB
P.B
1�B
>�B
�B
 vB
�B	�(B	�B	�5B	�|B	�B	�sB	��B	�aB	�6B	�oB	��B	��B	��B	��B	��B	HB	\CB	r�B	nIB	^jB	RTB	R:B	IRB	4�B	�B	�B	�B	�B��B��B�BB��B��B�!B�sB��BāB��B�YB��B�-B�-B�4B�(B��B��B�B�oB�`G�O�By�B�~B��B��B�EB��B�B��B��B��B�oB�B~]Bw2B�#B�=B��B��B��B�9BcBx�B|B��B~�Bs�G�O�BL0B6�Bg�B^�B^OBNVBM�BYBN�BN�BF�BN<BTBUgBX�B_�B\Bb�B\�BS[BQ4BS�BR BMPBFtBFYBE�BC�B]�B`�B_�B`�Bc�Bc�Bb�B^G�O�B=�BLJB[�B]�BZ�BZ�BZ�BX�B[�BV�B^�B[WB]/Be�BbB^5B`\Bi*Bh>BkBc�Bn�BnBp�BnBmCBoBs�Bs�BmCBo5Bm)BqBlWBe�Bb�Bk�BwfByXB.B�;B~]BcB�OB|�B|�B}B��B}�B}�B��B��B�B��B�B�xB�
B�7B�]B�BB��B��B�qB��B��B��B��B��B��B��B��B�gB�=B�DB�JB�\BѝBרBۦB��B��B��B�/B�B�`B�BB	 OB	[B	{B	fB	�B	
�B	�B	�B	�B	�B	�B	#B	%,B	$ZB	'�B	1[B	2�B	7�B	9�B	A�B	C�B	I�B	K)B	OB	PB	SB	V9B	ZQB	\]B	^jB	c�B	ezB	ezB	e�B	f�B	k�B	k�B	m�B	o�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	x�B	y	B	{B	�B	�B	�B	�xB	�dB	�jB	�\B	��B	��B	�}B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�RB	�tB	��B	�$B	�"B	�"B	�CB	�B	�OB	�wB	�FB	�^B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�PB	�B	�&B	�2B	�B	�B	�B	�+B	�+B	�+B	�+B	�?B	�_B	�WB	�dB	�jB	�jB	�vB	�vB	�bB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	� B	��B	��B	��B	��B	�AB	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�$B	�B	�B	�B	�B	�DB	�jB	�^B	�"B	�.B
 B
;B
 B
 B	�HB	�.B
AB
3B
+B
1B
EB
YB
fB

rB
xB

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
 �G�O�B
�B
�B
�B
;B
#�B
#�B
# B
!�B
!�B
# B
$G�O�B
%B
%�G�O�B
"NB
%B
'�B
(
B
'B
(�B
)B
($G�O�B
%,G�O�B
$&B
'B
)B
($B
($B
(
B
'B
'8B
)*B
+6B
+6B
+6B
+B
,"B
+B
+6B
*0B
)DB
,"B
-)B
,"B
,=G�O�B
*KB
,=B
./B
./B
/5B
./B
-CG�O�B
./B
.IB
./B
-CB
+kB
,qB
1AB
2-B
33B
2GB
1AB
1[B
0UB
0UB
2GB
3hB
2GB
2aB
3MB
3�B
5ZB
5?B
5ZB
5?B
4TB
4TB
4TB
4nB
3hG�O�B
/�B
3�B
7fB
6zG�O�B
5tB
7fB
8lB
9rB
9rB
9�B
9XB
:^B
:^B
:^B
:xB
9XB
9XB
9rB
9rG�O�G�O�B
4�B
:^B
;dB
;dB
;B
:xB
9�B
9�B
:�B
:�B
;�B
;�B
=�B
<�B
=�B
>�B
>�B
?}B
>wB
>�B
>�B
=�B
=�B
=�B
<�B
;�B
<�B
=�B
=�B
?�B
?�B
>�B
>�B
?�B
A�B
A�B
@�B
D�B
D�B
E�B
E�B
D�G�O�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
E�B
G�B
H�B
H�B
H�B
G�B
G�B
G�B
I�B
K�B
J�B
J�B
K�B
K�B
K�B
J�B
KB
J�B
L�B
K�B
L�B
M�B
L�B
K�B
K�B
MB
MB
O�B
N�B
N�B
NB
O�B
P�B
PB
P�B
O�B
Q B
Q B
QB
Q B
QB
RB
Q B
Q B
Q B
Q B
Q B
PB
Q B
QB
QB
SB
S&B
S�B
T,B
T,B
TB
TB
SB
R:B
RTB
VB
VB
VB
U2B
W
B
W$B
W$B
VB
UB
TFB
VB
U2B
V9B
WsG�O�B
XEB
XEB
YKB
\)B
\CB
]/B
\]B
\CB
[WB
[WB
\]B
^5B
_VB
_;B
^5B
^OB
^OB
]IB
]dB
]IB
_VB
_VB
`BB
`BB
_VB
_VB
`BB
`BB
`\B
`\B
`\B
`\B
_VB
^jB
_VB
_pB
^jB
^�B
^�B
_�B
c�B
dZB
dZB
dZB
dtB
dtB
cnB
cnB
dtB
ezB
ezB
e�B
e`B
ezB
ffB
ffB
ffB
f�B
ffB
ffB
ffB
f�B
f�B
f�B
ffB
f�B
f�B
gmB
gmB
gmB
g�B
gmB
gmB
gmB
gRB
f�B
f�B
f�B
e�B
e�B
gmB
f�B
f�B
e�B
f�B
h�B
h�B
h�B
g�B
i�B
i�B
h�B
i�B
j�B
i�B
k�B
j�B
j�B
k�B
k�B
j�B
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
k�B
j�B
j�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
n�B
n�B
m�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
x�B
w�B
w�B
x�B
x�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111114111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111411111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111141111111111141141111111141411111111111111111111114111111141111111111111111111111111111141111411111111111111144111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803220039192018032200391920180322003919201806221327472018062213274720180622132747201804050731432018040507314320180405073143  JA  ARFMdecpA19c                                                                20180318093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180318003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180318003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180318003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180318003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180318003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180318003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180318003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180318003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180318003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180318005515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180318153545  CV  JULD            G�O�G�O�F¡�                JM  ARSQJMQC2.0                                                                 20180319000000  CF  PSAL_ADJUSTED_QCB�  Dɀ G�O�                JM  ARCAJMQC2.0                                                                 20180321153919  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180321153919  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223143  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042747  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                