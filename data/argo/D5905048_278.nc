CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-04T21:35:13Z creation;2018-09-04T21:35:18Z conversion to V3.1;2019-12-19T07:29:36Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180904213513  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_278                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�~򻻻�1   @�~��O�@4��C�\��d`�W���1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��DxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\Dނ�D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�G�A�C�A�C�A�A�A�A�A�?}A�;dA�;dA�;dA�;dA�;dA�=qA�?}A�=qA�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�=qA�?}A�1'A���A���A�7LA��;A�hsA�ȴAٗ�A�hsA�7LA�(�A�$�A�oA؅A�+Aם�A�M�A�%A�1A���AԍPAӃAϰ!A���AʃAȡ�AǁA��#A�^5A��AŲ-A�A�1'A�7LA��`A°!A�ĜA��#A�A�A�$�A���A��+A���A��jA�33A��FA��jA�A��A�oA�p�A��`A�M�A���A�Q�A�VA�7LA��PA�ffA��FA�I�A��FA�VA��+A���A���A�v�A�S�A�"�A�33A��A�"�A��#A�`BA��hA��A�~�A��+A���A�dZA�1'A��HA���A�M�A��A�  A�ZA�(�A��^A��-A�"�A�;dA�1A�XA��A�A�A}��Az�RAw+Au�#At��At$�As33Aqx�Ao�
AnbAl��Aj�Ah�jAg+Ad��Aa�-A_��A[��AX�AXv�AU�AR�+AQ�AP-AN��AMl�AL�AL=qAK�PAJĜAI33AF�!AD�yABv�A@��A?�^A>�A=K�A=oA<=qA;�A9XA85?A7|�A6�uA5XA4�uA4  A3p�A1�A0�A.ffA+A*^5A)?}A(A�A'�TA'C�A&5?A%hsA%&�A$v�A$A#ƨA#p�A!��A v�A��A�`A�A��A��A��AA&�A�\A\)A��A�A�;A��A+A$�Av�A�A�7A
ffA	`BA�DA�A��Ap�A��A�+A  AS�AA�A��Al�A �jA n�@��@�\)@��R@���@�ȴ@���@�@���@�bN@�t�@�o@�@�?}@��j@�+@�bN@��-@��@��@��@�h@��`@�F@�/@�Ĝ@�r�@�b@�+@��@�E�@�/@�9X@�ƨ@߅@ݙ�@��@�&�@��@ҸR@�I�@���@�E�@��T@��@��`@�Z@���@�p�@�dZ@�z�@�K�@��#@��@�I�@�I�@+@ě�@�Ĝ@�p�@�O�@�j@�+@�~�@�@�hs@�9X@��@���@���@���@�V@�M�@�n�@�o@�dZ@��H@���@��/@��@�I�@�  @�t�@��@���@�v�@�@�X@��j@� �@��P@�33@�ȴ@���@�hs@��/@��F@��y@�n�@�%@���@���@��j@���@�33@��T@��-@��7@��/@�Q�@��w@���@���@�|�@�
=@��@���@���@��\@��R@���@��@�M�@�{@��@�G�@���@��@��9@�j@��@��;@��F@��P@��@�t�@�S�@��y@�=q@��@���@�`B@�7L@�V@�V@��/@�Ĝ@���@�Q�@��@��
@���@��P@�C�@�o@���@���@�^5@�5?@�$�@�{@��@��#@��-@��h@�`B@�X@�/@��@�V@�V@��`@��@�Q�@�1'@��@���@��w@��@�|�@�S�@��@���@��H@���@�n�@�$�@�@��@��h@�X@�O�@�O�@�O�@�G�@�/@�%@��@��/@��@���@���@���@��P@�|�@�l�@�@��!@���@���@��\@��+@�v�@��@��@�p�@�hs@�hs@�X@�7L@�&�@��@�V@���@��@� �@��
@��F@�|�@�dZ@�K�@�
=@���@�v�@�^5@�^5@�=q@��@�J@���@�7L@�r�@� �@���@��@��@��;@�|�@�S�@��@��@��H@�ȴ@�v�@�E�@���@�X@�?}@��@��@���@��@���@��/@���@��j@���@��m@��F@���@�+@�ȴ@�$�@���@�$�@���@�?}@�O�@��^@��T@���@���@���@���@��^@��@�G�@�V@���@���@��@��@��@�Z@� �@��@��F@�|�@�K�@��y@��+@�ff@�-@���@�&�@��u@�(�@�1@��@��
@��w@��P@�o@���@��+@�^5@�-@�$�@�J@���@��@��@��#@��h@�`B@��@�V@���@��u@�Z@K�@~�R@~��@�@�@~�R@~v�@~@}�h@}�@}�@}p�@}�@|j@{��@{"�@z�@{@z�\@y�7@y��@y�#@y��@yG�@y%@x��@y7L@y%@xĜ@x��@x�u@xb@w��@w�P@w;d@v��@up�@t�@tI�@sƨ@s��@s33@r��@r=q@q��@qX@q�@pr�@o�@o��@o
=@n@l�/@l9X@l(�@l(�@k�m@kC�@j�!@j=q@jJ@i�@iG�@hĜ@hr�@hb@g�@g�@f��@fv�@fV@f$�@e@e`B@e`B@e�@d�@d�/@d�j@d��@dj@c�
@cS�@b�!@b~�@bM�@a�7@aG�@`�`@`r�@`b@_��@_�@_l�@^�y@^v�@]��@]`B@]V@\�j@\z�@\9X@[�
@[��@[t�@Z�H@Z~�@ZM�@Y�7@Yx�@YX@YG�@Y%@X�`@X��@XA�@W��@W;d@V�@V$�@T��@T��@U�@T�@T��@S��@SC�@RM�@Q��@Q�7@Q&�@P�9@Pr�@P1'@O�;@O|�@O�@Nȴ@N�R@N��@Nv�@NV@N5?@M�@M@MO�@L��@L9X@L1@K��@K�m@K��@KC�@J�!@JM�@I��@I�@I�#@I��@I�^@I�^@I��@I��@I�7@I�@H��@HĜ@H��@G�@G\)@G;d@G+@F�R@F@E��@E�@E�@Ep�@Ep�@Ep�@E`B@E�@D�D@DZ@DI�@C�m@C��@CS�@Co@C@B�H@BM�@B=q@B�@A��@Ax�@A7L@A�@A%@@�`@@��@@�9@@A�@@  @?�;@?�;@?��@?�@?l�@?+@>�R@>5?@=��@=�@=p�@=`B@=V@<��@<I�@;�
@;��@;t�@;C�@;o@:�@:��@:�!@:��@9��@8��@8��@8A�@7�w@7+@6��@6�+@6E�@6{@5�@5@5��@5`B@4�/@4z�@49X@3ƨ@333@3@3@2�@2n�@1��@1�7@1G�@1G�@1&�@0Ĝ@0 �@0b@/��@/�@.��@.v�@.v�@.V@.E�@.5?@.E�@.@-��@-O�@-/@-�@-V@,�@,�D@,z�@,Z@+ƨ@+��@+S�@+o@+@*�H@*n�@*=q@)�@)�#@)�7@(��@(Ĝ@(��@(�u@(�u@(r�@(A�@'��@'�P@'|�@'l�@'K�@';d@';d@';d@'
=@&ȴ@&ff@&5?@&$�@%�T@%�-@%�h@%p�@%O�@$�/@$j@$(�@#��@#�
@#ƨ@#��@#C�@#33@#o@"��@"^5@"=q@!�#@!��@!X@!G�@!&�@ ��@ �`@ ��@ Ĝ@ ��@ �@ bN@ A�@  �@ b@�;@��@;d@��@��@��@��@ff@E�@5?@$�@�@@�h@?}@/@�@��@�@z�@I�@(�@�@��@�F@t�@dZ@dZ@dZ@dZ@S�@S�@C�@33@@��@�\@~�@J@��@��@��@X@7L@&�@&�@&�@�@��@�`@��@�9@�9@�9@��@�u@�u@�u@r�@  @��@�w@��@\)@ȴ@E�@@�T@@`B@/@V@��@��@�j@�D@z�@I�@1@��@��@��@��@�@�@�@S�@@�\@=q@�@�@J@��@�#@�7@hs@X@%@��@r�@A�@b@�;@��@�@��@|�@�@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�G�A�C�A�C�A�A�A�A�A�?}A�;dA�;dA�;dA�;dA�;dA�=qA�?}A�=qA�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�=qA�?}A�1'A���A���A�7LA��;A�hsA�ȴAٗ�A�hsA�7LA�(�A�$�A�oA؅A�+Aם�A�M�A�%A�1A���AԍPAӃAϰ!A���AʃAȡ�AǁA��#A�^5A��AŲ-A�A�1'A�7LA��`A°!A�ĜA��#A�A�A�$�A���A��+A���A��jA�33A��FA��jA�A��A�oA�p�A��`A�M�A���A�Q�A�VA�7LA��PA�ffA��FA�I�A��FA�VA��+G�O�G�O�A�v�A�S�A�"�A�33A��A�"�A��#A�`BA��hA��A�~�A��+A���A�dZA�1'A��HA���A�M�A��A�  A�ZA�(�A��^A��-A�"�A�;dA�1A�XA��A�A�A}��Az�RAw+Au�#At��At$�As33Aqx�Ao�
AnbAl��Aj�Ah�jAg+Ad��Aa�-A_��A[��AX�AXv�AU�AR�+AQ�AP-AN��AMl�AL�AL=qAK�PAJĜAI33AF�!AD�yABv�A@��A?�^A>�A=K�A=oA<=qA;�A9XA85?A7|�A6�uA5XA4�uA4  A3p�A1�A0�A.ffA+A*^5A)?}A(A�A'�TA'C�A&5?A%hsA%&�A$v�A$A#ƨA#p�A!��A v�A��A�`A�A��A��A��AA&�A�\A\)A��A�A�;A��A+A$�Av�A�A�7A
ffA	`BA�DA�A��Ap�A��A�+A  AS�AA�A��Al�A �jA n�@��@�\)@��R@���@�ȴ@���@�@���@�bN@�t�@�o@�@�?}@��j@�+@�bN@��-@��@��@��@�h@��`@�F@�/@�Ĝ@�r�@�b@�+@��@�E�@�/@�9X@�ƨ@߅@ݙ�@��@�&�@��@ҸR@�I�@���@�E�@��T@��@��`@�Z@���@�p�@�dZ@�z�@�K�@��#@��@�I�@�I�@+@ě�@�Ĝ@�p�@�O�@�j@�+@�~�@�@�hs@�9X@��@���@���@���@�V@�M�@�n�@�o@�dZ@��H@���@��/@��@�I�@�  @�t�@��@���@�v�@�@�X@��j@� �@��P@�33@�ȴ@���@�hs@��/@��F@��y@�n�@�%@���@���@��j@���@�33@��T@��-@��7@��/@�Q�@��w@���@���@�|�@�
=@��@���@���@��\@��R@���@��@�M�@�{@��@�G�@���@��@��9@�j@��@��;@��F@��P@��@�t�@�S�@��y@�=q@��@���@�`B@�7L@�V@�V@��/@�Ĝ@���@�Q�@��@��
@���@��P@�C�@�o@���@���@�^5@�5?@�$�@�{@��@��#@��-@��h@�`B@�X@�/@��@�V@�V@��`@��@�Q�@�1'@��@���@��w@��@�|�@�S�@��@���@��H@���@�n�@�$�@�@��@��h@�X@�O�@�O�@�O�@�G�@�/@�%@��@��/@��@���@���@���@��P@�|�@�l�@�@��!@���@���@��\@��+@�v�@��@��@�p�@�hs@�hs@�X@�7L@�&�@��@�V@���@��@� �@��
@��F@�|�@�dZ@�K�@�
=@���@�v�@�^5@�^5@�=q@��@�J@���@�7L@�r�@� �@���@��@��@��;@�|�@�S�@��@��@��H@�ȴ@�v�@�E�@���@�X@�?}@��@��@���@��@���@��/@���@��j@���@��m@��F@���@�+@�ȴ@�$�@���@�$�@���@�?}@�O�@��^@��T@���@���@���@���@��^@��@�G�@�V@���@���@��@��@��@�Z@� �@��@��F@�|�@�K�@��y@��+@�ff@�-@���@�&�@��u@�(�@�1@��@��
@��w@��P@�o@���@��+@�^5@�-@�$�@�J@���@��@��@��#@��h@�`B@��@�V@���@��u@�Z@K�@~�R@~��@�@�@~�R@~v�@~@}�h@}�@}�@}p�@}�@|j@{��@{"�@z�@{@z�\@y�7@y��@y�#@y��@yG�@y%@x��@y7L@y%@xĜ@x��@x�u@xb@w��@w�P@w;d@v��@up�@t�@tI�@sƨ@s��@s33@r��@r=q@q��@qX@q�@pr�@o�@o��@o
=@n@l�/@l9X@l(�@l(�@k�m@kC�@j�!@j=q@jJ@i�@iG�@hĜ@hr�@hb@g�@g�@f��@fv�@fV@f$�@e@e`B@e`B@e�@d�@d�/@d�j@d��@dj@c�
@cS�@b�!@b~�@bM�@a�7@aG�@`�`@`r�@`b@_��@_�@_l�@^�y@^v�@]��@]`B@]V@\�j@\z�@\9X@[�
@[��@[t�@Z�H@Z~�@ZM�@Y�7@Yx�@YX@YG�@Y%@X�`@X��@XA�@W��@W;d@V�@V$�@T��@T��@U�@T�@T��@S��@SC�@RM�@Q��@Q�7@Q&�@P�9@Pr�@P1'@O�;@O|�@O�@Nȴ@N�R@N��@Nv�@NV@N5?@M�@M@MO�@L��@L9X@L1@K��@K�m@K��@KC�@J�!@JM�@I��@I�@I�#@I��@I�^@I�^@I��@I��@I�7@I�@H��@HĜ@H��@G�@G\)@G;d@G+@F�R@F@E��@E�@E�@Ep�@Ep�@Ep�@E`B@E�@D�D@DZ@DI�@C�m@C��@CS�@Co@C@B�H@BM�@B=q@B�@A��@Ax�@A7L@A�@A%@@�`@@��@@�9@@A�@@  @?�;@?�;@?��@?�@?l�@?+@>�R@>5?@=��@=�@=p�@=`B@=V@<��@<I�@;�
@;��@;t�@;C�@;o@:�@:��@:�!@:��@9��@8��@8��@8A�@7�w@7+@6��@6�+@6E�@6{@5�@5@5��@5`B@4�/@4z�@49X@3ƨ@333@3@3@2�@2n�@1��@1�7@1G�@1G�@1&�@0Ĝ@0 �@0b@/��@/�@.��@.v�@.v�@.V@.E�@.5?@.E�@.@-��@-O�@-/@-�@-V@,�@,�D@,z�@,Z@+ƨ@+��@+S�@+o@+@*�H@*n�@*=q@)�@)�#@)�7@(��@(Ĝ@(��@(�u@(�u@(r�@(A�@'��@'�P@'|�@'l�@'K�@';d@';d@';d@'
=@&ȴ@&ff@&5?@&$�@%�T@%�-@%�h@%p�@%O�@$�/@$j@$(�@#��@#�
@#ƨ@#��@#C�@#33@#o@"��@"^5@"=q@!�#@!��@!X@!G�@!&�@ ��@ �`@ ��@ Ĝ@ ��@ �@ bN@ A�@  �@ b@�;@��@;d@��@��@��@��@ff@E�@5?@$�@�@@�h@?}@/@�@��@�@z�@I�@(�@�@��@�F@t�@dZ@dZ@dZ@dZ@S�@S�@C�@33@@��@�\@~�@J@��@��@��@X@7L@&�@&�@&�@�@��@�`@��@�9@�9@�9@��@�u@�u@�u@r�@  @��@�w@��@\)@ȴ@E�@@�T@@`B@/@V@��@��@�j@�D@z�@I�@1@��@��@��@��@�@�@�@S�@@�\@=q@�@�@J@��@�#@�7@hs@X@%@��@r�@A�@b@�;@��@�@��@|�@�@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�\B	�\B	�VB	�PB	�DB	�B	}�B	r�B	�=B	�oB	��B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	p�B	��B	��B	�B
,B
l�B
�bB
�jB
��BPB#�B-BR�BZBS�BXB|�B�7B�RB�?B�RB�-B�1B��B�dB��BĜB��B��B�FBbNB\)Bo�B|�B��B1B.B0!B7LB)�BBuB7LB1'B��B��B�Bo�B$�B
�B
�RB
�dB
��B
�B
v�B
^5B
G�B
!�B
{B
#�B
B	�B	�TB	��B	�?B	��B	�^B	�PB	�1B	�JB	{�B	YB	VB	6FB	0!B	-B	bB	%�B	$�B	�B	�B	B��B�B�B�;B��BĜB�dB��B�B�\B�{B�RB�{B�bB�B��B��B�B�FB�9B�B��B��B�PB��B��B�B�XBB�XB��BB�qB�?B��BŢBĜB�jBŢBŢB��B�B�B��B��B�qBĜBƨB��B��BƨB��B�
B��B��B��BƨB�'B��B�uB�7Bk�B\)BaHBo�Bt�Bm�Bk�BiyBp�Bo�By�Bu�Bl�BdZB[#BjBn�BcTBp�Bw�B�B�B�%B�B|�B� By�Bs�BjBn�Bx�B|�Bp�By�B�PB��B��B��B��B��B��B��B��B��B��B��B��B�1B�1B��B��B��B�B�B��B��B�LB�XB�XB�XBŢB��B��B��B�B��B��B�!B�{B�3B��B��B��B�B�?B�3B�RB�3B�!B�3B�'B��B�9B�FB�XBĜB��B�B��B	  B	  B	B��B��B	B	%B	B	  B��B��B�B�B��B��B	B	JB	�B	bB	PB	PB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	�B	�B	�B	 �B	�B	!�B	�B	 �B	'�B	$�B	.B	1'B	0!B	)�B	,B	&�B	/B	/B	.B	49B	7LB	?}B	A�B	@�B	?}B	D�B	F�B	H�B	J�B	N�B	S�B	Q�B	P�B	R�B	W
B	VB	]/B	bNB	bNB	cTB	e`B	jB	m�B	m�B	o�B	o�B	o�B	m�B	m�B	t�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�PB	�JB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�3B	�FB	�RB	�XB	�XB	�RB	�RB	�RB	�RB	�RB	�LB	�LB	�qB	�}B	��B	��B	��B	�wB	��B	ŢB	ƨB	ŢB	ŢB	ĜB	B	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�B	�B	�#B	�B	�)B	�/B	�)B	�B	�)B	�#B	�/B	�NB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
+B
+B
%B
B
B
%B
+B
	7B
	7B
1B
+B
B
+B
	7B
JB
\B
PB
DB
uB
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
!�B
!�B
"�B
#�B
#�B
#�B
%�B
%�B
$�B
#�B
"�B
"�B
#�B
&�B
&�B
$�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
(�B
(�B
'�B
)�B
+B
,B
-B
-B
-B
-B
.B
-B
,B
.B
+B
/B
/B
/B
.B
/B
.B
.B
.B
/B
0!B
/B
-B
49B
5?B
49B
33B
2-B
1'B
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
8RB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
:^B
<jB
>wB
?}B
?}B
>wB
=qB
=qB
>wB
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
A�B
@�B
A�B
B�B
A�B
@�B
@�B
C�B
C�B
B�B
A�B
C�B
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
E�B
E�B
E�B
F�B
F�B
F�B
E�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
F�B
H�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
G�B
H�B
J�B
J�B
J�B
I�B
I�B
I�B
J�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
K�B
H�B
I�B
M�B
K�B
L�B
M�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
P�B
Q�B
Q�B
P�B
S�B
T�B
S�B
Q�B
R�B
R�B
T�B
VB
T�B
S�B
R�B
VB
T�B
T�B
VB
XB
YB
YB
YB
YB
YB
XB
W
B
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
YB
[#B
[#B
[#B
\)B
\)B
ZB
\)B
[#B
\)B
[#B
ZB
^5B
^5B
_;B
_;B
^5B
^5B
]/B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
_;B
`BB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
bNB
cTB
dZB
e`B
e`B
dZB
ffB
ffB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
cTB
e`B
gmB
ffB
e`B
e`B
gmB
gmB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
iyB
jB
k�B
k�B
k�B
jB
jB
jB
iyB
iyB
hsB
iyB
iyB
hsB
iyB
k�B
k�B
jB
k�B
l�B
m�B
m�B
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
l�B
k�B
jB
k�B
l�B
k�B
jB
iyB
jB
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
n�B
o�B
p�B
r�B
r�B
r�B
q�B
q�B
p�B
q�B
r�B
q�B
p�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�bB	�bB	�bB	�bB	�}B	�bB	�}B	�}B	�}B	�}B	�vB	�\B	�pB	�jB	�^B	��B	B	uZB	��B	�[B	�vB	��B	��B	�B	�B	��B	��B	�!B	��B	��B	��B	��B	�B	�B	�zB	��B	��B	y	B	�KB	�?B	�%B
./B
m�B
�hB
�<B
��B�B%zB/ BS�B[#BV�B\CB�B�^B��B��B�DB�%B��B��B�0B�oBāB�oB�MB�JBg8B_Bp;B|�B��BB.cB1�B8lB,B1B[B6�G�O�G�O�B�VB�gBshB-�B
�B
�HB
�]B
�LB
��B
y�B
`�B
JrB
&2B
?B
$�B
SB	��B	�2B	өB	��B	�xB	��B	� B	��B	��B	B	]/B	Y1B	:�B	3�B	0�B	aB	'8B	&B	 �B	�B	mB	B��B�B�BөB�B�wB��B��B�aB��B��B��B�uB�5B��B��B��B��B�?B�/B�yB�	B��B�	B��B��B��B��B��B�<B��B�.B�fB��BƨB��B�BƨBƎB��B�[B�/B��B�,B��B�B��B҉B��B�B��B׍B��BуB�oBǮB�hB��B�B��Bn�B_�Bc�Bp�Bu�Bn�Bl�BkBqvBp�BzBv`Bm�Be�B]IBk6BoOBeBq�Bx�B��B��B��B��B}�B��B{0BuZBl�BpUBy�B}�Br�B{0B��B��B��B�$B��B��B�sB��B�eB��B��B��B��B�rB�	B��B��B��B��B��B��B�tB��B��B��B�B�B�)B˒BЗB�_B�{B�-B�aB��B��B��B�fB��B��B��B��B��B��B�[B�TB��B��B�B�2B��B��B��B�'B��B	  B��B	GB��B��B	�B	�B	�B	 �B��B��B��B�B��B��B	�B	B	mB	�B	B	B	�B	�B	�B	�B	B	�B	�B	+B	�B	
B	�B	�B	B	 B	!B	�B	"NB	 �B	!|B	(XB	%�B	./B	1AB	0UB	*�B	,�B	'�B	/5B	/iB	.�B	4�B	7�B	?�B	A�B	@�B	?�B	D�B	F�B	H�B	J�B	N�B	S�B	R B	QNB	S@B	W?B	V�B	]dB	bhB	b�B	c�B	e�B	j�B	m�B	m�B	o�B	o�B	o�B	m�B	nB	uB	y$B	|B	B	� B	�-B	�GB	�3B	�MB	�SB	�_B	��B	�xB	�jB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�
B	�$B	�DB	�*B	�B	�"B	�6B	�CB	�IB	�GB	�MB	�hB	�`B	�RB	�XB	�XB	�lB	�lB	��B	�lB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ŢB	ƨB	żB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�	B	�B	�"B	��B	�B	� B	� B	�.B	�.B	�B	�B	�B	�B	�B	�B	�@B	�[B	ЗB	�?B	�7B	�#B	�#B	�7B	�_B	�WB	�QB	�]B	�IB	�]B	�kB	�]B	یB	�~B	�hB	�nB	�tB	�B	�tB	�zB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	��B	�	B	�$B	�B	�	B	�	B	�B	�+B	��B	�B	�-B	�%B	�9B	�B	�	B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�	B	�B	�B	�B	�B	�0B	�B	�dB
 4B
B
+B
?B
SB
9B
YB
EB
	RB
	7B
KB
_B
�B
zB
	lB
dB
\B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
!�B
!�B
"�B
#�B
#�B
$B
%�B
%�B
$�B
#�B
#B
#B
$&B
'B
'B
%,B
(
B
($B
($B
($B
*B
*B
*B
)DB
)*B
(>B
*0B
+B
,"B
-)B
-CB
-)B
-)B
./B
-CB
,=B
./B
+QB
/B
/5B
/5B
./B
/5B
./B
.IB
.cB
/OB
0UB
/iB
-]B
49B
5ZB
4TB
3hB
2|B
1vB
1vB
3�B
5ZB
5tB
5tB
6`B
6`B
6zB
6zB
7fB
8lB
:xB
:�B
:xB
:xB
:xB
;B
;B
;�B
:�B
<�B
>�B
?�B
?�B
>�B
=�B
=�B
>�B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
A�B
@�B
A�B
B�B
A�B
@�B
@�B
C�B
C�B
B�B
A�B
C�B
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
E�B
E�B
E�B
F�B
F�B
F�B
E�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
F�B
H�B
I�B
I�B
I�B
H�B
H�B
G�B
F�B
G�B
H�B
J�B
J�B
J�B
I�B
I�B
I�B
J�B
MB
MB
MB
L�B
M�B
M�B
L�B
K�B
IB
I�B
M�B
LB
MB
NB
O�B
PB
Q B
RB
RB
R B
Q B
Q B
PB
QB
RB
RB
QB
TB
UB
T,B
R:B
S&B
S&B
UB
VB
UB
T,B
S&B
VB
U2B
U2B
V9B
X+B
Y1B
Y1B
YB
YB
YB
X+B
W?B
X+B
ZB
ZB
Z7B
Z7B
Z7B
[#B
[=B
YKB
[=B
[=B
[=B
\]B
\CB
ZQB
\CB
[=B
\CB
[WB
Z�B
^OB
^OB
_;B
_;B
^OB
^OB
]dB
^OB
`BB
`BB
`vB
`BB
`BB
`BB
_VB
^jB
_VB
`\B
`\B
`\B
`vB
abB
abB
`\B
_pB
`�B
abB
bhB
bhB
bNB
abB
abB
bNB
bhB
a|B
abB
bhB
bhB
c�B
dtB
ezB
ezB
dtB
ffB
ffB
ezB
ezB
ezB
e�B
ezB
ezB
ezB
e�B
dtB
c�B
ezB
gmB
ffB
ezB
ezB
gmB
g�B
gmB
f�B
f�B
g�B
g�B
hsB
h�B
gmB
h�B
h�B
h�B
i�B
i�B
i�B
h�B
i�B
jB
k�B
k�B
k�B
jB
jB
jB
i�B
i�B
h�B
i�B
i�B
h�B
i�B
k�B
k�B
j�B
k�B
l�B
m�B
m�B
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
l�B
k�B
j�B
k�B
l�B
k�B
j�B
i�B
j�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
n�B
o�B
p�B
r�B
r�B
r�B
q�B
q�B
p�B
q�B
r�B
q�B
p�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809090035502018090900355020180909003550201809090200302018090902003020180909020030201809100027102018091000271020180910002710  JA  ARFMdecpA19c                                                                20180905063512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180904213513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180904213516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180904213517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180904213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180904213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180904213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180904213517  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180904213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180904213517  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180904213518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180904213518                      G�O�G�O�G�O�                JA  ARUP                                                                        20180904215609                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180905153459  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180908153550  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180908153550  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180908170030  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180909152710  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                