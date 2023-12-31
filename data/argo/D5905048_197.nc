CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-05T00:35:26Z creation;2018-01-05T00:35:30Z conversion to V3.1;2019-12-19T07:48:27Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180105003526  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_197                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�B3ɓ� 1   @�B4����@4R� ѷ�ds���l�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؃3D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @o\)@�z�@�z�A=qA:=qAZ=qAz=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B&�\B.�\B6�\B>�\BF�\BN�\BV�\B^�\Bf�\Bn�\Bv�\B~�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D h�D ��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D	h�D	��D
h�D
��Dh�D��Dh�D��Db�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D h�D ��D!h�D!��D"h�D"��D#h�D#��D$h�D$��D%h�D%��D&h�D&��D'h�D'��D(h�D(��D)h�D)��D*h�D*��D+h�D+��D,h�D,��D-h�D-��D.h�D.��D/h�D/��D0h�D0��D1h�D1��D2h�D2��D3h�D3��D4h�D4��D5h�D5��D6h�D6��D7h�D7��D8h�D8��D9h�D9��D:h�D:��D;h�D;��D<h�D<��D=h�D=��D>h�D>��D?h�D?��D@h�D@��DAh�DA��DBh�DB��DCh�DC��DDh�DD��DEh�DE��DFh�DF��DGh�DG��DHh�DH��DIh�DI��DJh�DJ��DKh�DK��DLh�DL��DMh�DM��DNh�DN��DOh�DO��DPh�DP��DQh�DQ��DRh�DR��DSh�DS��DTh�DT��DUh�DU��DVh�DV��DWh�DW��DXh�DX��DYh�DY��DZh�DZ��D[h�D[��D\h�D\��D]h�D]��D^h�D^��D_h�D_��D`h�D`��Dah�Da��Dbh�Db��Dch�Dc��Ddh�Dd��Deh�De��Dfh�Df��Dgh�Dg��Dhh�Dh��Dih�Di��Djh�Dj��Dkh�Dk��Dlh�Dl��Dmh�Dm��Dnh�Dn��Doh�Do��Dph�Dp��Dqh�Dq��Drh�Dr��Dsh�Ds��Dth�Dt��Duh�Du��Dvh�Dv��Dwh�Dw��Dxh�Dx��Dyh�Dy��Dzh�Dz��D{h�D{��D|h�D|��D}h�D}��D~h�D~��Dh�D��D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�7�D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�w�D��{D��{D�4{D�w�D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D´{D��{D�4{D�t{Dô{D��{D�4{D�t{DĴ{D��{D�4{D�t{DŴ{D��{D�4{D�t{Dƴ{D��{D�4{D�t{DǴ{D��{D�4{D�t{Dȴ{D��{D�4{D�t{Dɴ{D��{D�4{D�t{Dʴ{D��{D�4{D�t{D˴{D��{D�4{D�t{D̴{D��{D�4{D�t{Dʹ{D��{D�4{D�t{Dδ{D��{D�4{D�t{Dϴ{D��{D�4{D�t{Dд{D��{D�4{D�t{DѴ{D��{D�4{D�t{DҴ{D��{D�4{D�t{DӴ{D��{D�4{D�t{DԴ{D��{D�4{D�t{Dմ{D��{D�4{D�t{Dִ{D��{D�4{D�t{D״{D��{D�4{D�w�Dش{D��{D�4{D�t{Dٴ{D��{D�4{D�t{Dڴ{D��{D�4{D�t{D۴{D��{D�4{D�t{Dܴ{D��{D�4{D�t{Dݴ{D��{D�4{D�t{D޴{D��{D�4{D�t{Dߴ{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D���D�4{D�t{D�{D��{D�7�D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�HD��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D��{D��{D�4{D�t{D�{D��{D�4{D�t{D﷮D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�qH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɸRAɶFAɴ9Aɴ9Aɴ9Aɴ9Aɰ!Aɰ!Aɲ-Aɴ9AɸRAɶFAɴ9AɶFAɶFAɶFAɶFAɶFAɴ9AɸRAɺ^Aɺ^A���A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���Aɴ9Aȕ�A�(�A�VA\A�ĜA�ȴA�bNA�I�A�l�A�{A�A���A�\)A�K�A�^5A��A�ZA�A���A��HA�
=A��!A��TA��A�oA�/A��A��PA��A�?}A���A�r�A�
=A���A�G�A��PA�M�A���A��-A��A�ƨA��TA�~�A�{A�+A��+A�I�A�&�A��A�hsA�A�A��A���A��hA�ȴA�ƨA�E�A�hsA��A��A���A���A�~�A��A��PA��9A�x�A��A��A�mA}XAy+Au�#As�mAp��An9XAl��Ai�-Ah �Af �Ae��Ad�A`�yA_�7A_
=A^I�A\�`AY�-AVĜAT�ATbARr�AP�/AN�AL�AIp�AF��ADZACVAA��A@�`A?C�A>I�A=�TA=\)A;�^A:9XA9x�A9XA8��A8I�A6��A5�A5?}A4Q�A3G�A2~�A21'A1��A/x�A.�A,�A*�DA*�A(�A';dA%�A$�+A!|�A��AdZA�uA��A��AoA
=A��At�A
=A�AVA(�AC�A��A��A9XA1A�TA7LAr�A�jAQ�A�AoA
=qA	�FA	33A�RA{A��A��Ap�A?}A�A�A��A�AJ@��@�E�@�@���@�V@���@�@���@@�=q@��@�bN@���@�r�@��;@�|�@ꗍ@��@�P@��T@���@�9X@�O�@�1@�E�@��#@�r�@�\)@��y@�^5@���@�1'@�v�@�V@��m@��@���@���@�t�@�+@͙�@�%@̴9@̋D@���@�|�@�v�@ə�@�O�@�%@�9X@ǶF@�~�@ź^@ēu@���@�5?@���@��j@���@�1'@�|�@�@���@�ff@�ff@��@�V@�bN@���@���@��@��^@��`@�Z@��;@�t�@��!@�J@���@���@�p�@�X@��u@�\)@��y@�M�@��@��^@��h@�p�@�&�@�A�@��;@�S�@��H@�ff@�M�@�{@���@�?}@�Ĝ@�bN@�9X@�b@� �@�(�@��w@�ȴ@�ff@�=q@��@��^@�X@��@���@���@��D@�Q�@�1'@�(�@�b@�|�@��y@��\@�V@��#@�X@��/@��j@�j@��@�(�@��m@���@�S�@��@���@���@���@���@��@�33@���@��+@�{@���@��u@�1'@��
@��@���@�l�@�|�@�K�@�n�@�$�@���@�7L@���@���@��u@��u@���@��@�r�@�A�@�(�@���@���@��P@�S�@�33@�o@��H@���@�V@�5?@��@���@��@��u@�Z@�A�@�ƨ@�S�@�o@�ȴ@��\@��R@�v�@�-@�$�@��@�{@���@�O�@�%@�Ĝ@��9@�r�@�b@�  @��@��
@��@�l�@�\)@�S�@�;d@�o@���@���@���@�v�@�ff@�{@��@��T@��^@�G�@��j@�r�@�Z@� �@�  @��@��@�|�@��P@���@��@�l�@�K�@�o@���@�v�@�E�@��@�p�@�/@�/@���@�bN@�1@�ƨ@���@�\)@�;d@��@��@��+@�M�@�@��#@��^@���@�x�@�7L@�V@�Ĝ@�1'@��m@��w@��@�t�@�C�@�"�@�@���@�V@��@���@���@��@�@��7@�7L@���@��@�z�@�Q�@��@���@�t�@�S�@�C�@�;d@�33@��@�
=@���@��y@��!@�ff@��T@��-@��h@��7@�x�@�X@�%@��@��j@�j@�j@�j@�bN@�j@�bN@�Z@�bN@�I�@��@;d@�@
=@
=@
=@~��@~��@~@}?}@|j@|�@{ƨ@{��@{S�@{@z��@zn�@z-@yx�@x��@x�@w�;@w�P@w|�@wK�@w
=@vE�@u�T@u�-@u�@uO�@t��@t�/@tZ@t�@sƨ@s�@sS�@s33@r�@rn�@q�^@qX@q7L@p��@pQ�@o�@oK�@nȴ@n�+@n$�@m�T@m/@l�@l�@k��@k33@j��@j^5@i��@i�@h1'@g|�@g�@f�R@fE�@e�-@e?}@d�/@dz�@d�@cƨ@c�@c33@b��@b=q@bJ@a��@a�^@aX@`�`@`Ĝ@`�9@`r�@`A�@_��@_;d@_;d@_�@^�y@^ȴ@^��@^ff@]�-@]/@\�@\�j@[��@[�@[dZ@[@Z��@Z-@Y�^@YG�@Y%@X��@XbN@W��@Wl�@W;d@V��@Vȴ@V�R@V5?@U��@Up�@U/@UV@T�@T�/@T�j@T9X@Sƨ@St�@SS�@S33@So@R��@Rn�@R=q@R�@Q��@Qhs@Q7L@Q&�@Q�@Q%@P�`@P�u@Pb@P  @O�P@N�y@Nff@N$�@M�T@M�-@Mp�@M/@L��@L�@Lz�@Lj@LZ@K��@K�F@K��@Kt�@KdZ@K33@J�@J�!@JM�@J-@I�@I�^@IX@H�`@Hr�@H  @G��@F��@FV@FE�@F5?@F$�@E�@E@E�h@EV@D�j@D�j@D�@D��@D�D@D9X@C�F@CS�@B�@B��@Bn�@B=q@BJ@A��@A��@A�7@AX@A&�@A&�@A�@A%@@��@@Ĝ@@�u@@bN@@1'@@b@?�@?��@?��@?��@?�P@?;d@>�y@>�+@>E�@=�h@=V@<�j@<�j@<��@<�D@<j@<j@<�@;"�@:�H@:��@:�\@:^5@:-@9�@8�@8A�@8  @7�@7\)@6��@6��@6��@6��@6V@65?@5�T@5�@5/@4�/@4�@4j@3��@3�F@3"�@2~�@1��@1��@1��@1X@1�@0��@0�@01'@/�@/�@/K�@/�@.�@.��@.ff@.5?@.$�@.@-�T@-@-@-��@-?}@-�@,�/@,�D@,1@+��@+��@+S�@+o@*��@*M�@*-@)��@)x�@)x�@)x�@)X@)�@(�`@(Ĝ@(�@(b@'l�@';d@';d@&��@&v�@&5?@%�T@%�@%��@%p�@%O�@%O�@%/@$��@$�@$�/@$�@$j@$(�@$1@#��@#ƨ@#��@#��@#��@#��@#t�@"�@"n�@"=q@"J@!�@!��@!��@!G�@ ��@ �9@ bN@  �@   @��@�P@l�@l�@\)@K�@;d@
=@ȴ@�R@v�@E�@@�T@@�-@�@`B@?}@�@��@�D@z�@9X@��@�F@��@��@S�@"�@o@@�@�!@^5@�@��@��@�7@�7@x�@x�@x�@X@�@�@%@��@��@��@�`@Ĝ@Ĝ@�9@bN@bN@ �@K�@;d@+@�@��@��@�+@ff@V@{@@@@�@�T@��@��@�h@p�@�@�@��@��@z�@Z@(�@�@��@�F@��@33@@��@M�@=q@-@J@J@��@�@�#@��@x�@G�@G�@7L@%@�`@Ĝ@�9@��@��@�u@�@Q�@ �@�@�;@��@�@�@��@�+@ff@5?@$�@@��@`B@/@�@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɸRAɶFAɴ9Aɴ9Aɴ9Aɴ9Aɰ!Aɰ!Aɲ-Aɴ9AɸRAɶFAɴ9AɶFAɶFAɶFAɶFAɶFAɴ9AɸRAɺ^Aɺ^A���A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���Aɴ9Aȕ�A�(�A�VA\A�ĜA�ȴA�bNA�I�A�l�A�{A�A���A�\)A�K�A�^5A��A�ZA�A���A��HA�
=A��!A��TA��A�oA�/A��A��PA��A�?}A���A�r�A�
=A���A�G�A��PA�M�A���A��-A��A�ƨA��TA�~�A�{A�+A��+A�I�A�&�A��A�hsA�A�A��A���A��hA�ȴA�ƨA�E�A�hsA��A��A���A���A�~�A��A��PA��9A�x�A��A��A�mA}XAy+Au�#As�mAp��An9XAl��Ai�-Ah �Af �Ae��Ad�A`�yA_�7A_
=A^I�A\�`AY�-AVĜAT�ATbARr�AP�/AN�AL�AIp�AF��ADZACVAA��A@�`A?C�A>I�A=�TA=\)A;�^A:9XA9x�A9XA8��A8I�A6��A5�A5?}A4Q�A3G�A2~�A21'A1��A/x�A.�A,�A*�DA*�A(�A';dA%�A$�+A!|�A��AdZA�uA��A��AoA
=A��At�A
=A�AVA(�AC�A��A��A9XA1A�TA7LAr�A�jAQ�A�AoA
=qA	�FA	33A�RA{A��A��Ap�A?}A�A�A��A�AJ@��@�E�@�@���@�V@���@�@���@@�=q@��@�bN@���@�r�@��;@�|�@ꗍ@��@�P@��T@���@�9X@�O�@�1@�E�@��#@�r�@�\)@��y@�^5@���@�1'@�v�@�V@��m@��@���@���@�t�@�+@͙�@�%@̴9@̋D@���@�|�@�v�@ə�@�O�@�%@�9X@ǶF@�~�@ź^@ēu@���@�5?@���@��j@���@�1'@�|�@�@���@�ff@�ff@��@�V@�bN@���@���@��@��^@��`@�Z@��;@�t�@��!@�J@���@���@�p�@�X@��u@�\)@��y@�M�@��@��^@��h@�p�@�&�@�A�@��;@�S�@��H@�ff@�M�@�{@���@�?}@�Ĝ@�bN@�9X@�b@� �@�(�@��w@�ȴ@�ff@�=q@��@��^@�X@��@���@���@��D@�Q�@�1'@�(�@�b@�|�@��y@��\@�V@��#@�X@��/@��j@�j@��@�(�@��m@���@�S�@��@���@���@���@���@��@�33@���@��+@�{@���@��u@�1'@��
@��@���@�l�@�|�@�K�@�n�@�$�@���@�7L@���@���@��u@��u@���@��@�r�@�A�@�(�@���@���@��P@�S�@�33@�o@��H@���@�V@�5?@��@���@��@��u@�Z@�A�@�ƨ@�S�@�o@�ȴ@��\@��R@�v�@�-@�$�@��@�{@���@�O�@�%@�Ĝ@��9@�r�@�b@�  @��@��
@��@�l�@�\)@�S�@�;d@�o@���@���@���@�v�@�ff@�{@��@��T@��^@�G�@��j@�r�@�Z@� �@�  @��@��@�|�@��P@���@��@�l�@�K�@�o@���@�v�@�E�@��@�p�@�/@�/@���@�bN@�1@�ƨ@���@�\)@�;d@��@��@��+@�M�@�@��#@��^@���@�x�@�7L@�V@�Ĝ@�1'@��m@��w@��@�t�@�C�@�"�@�@���@�V@��@���@���@��@�@��7@�7L@���@��@�z�@�Q�@��@���@�t�@�S�@�C�@�;d@�33@��@�
=@���@��y@��!@�ff@��T@��-@��h@��7@�x�@�X@�%@��@��j@�j@�j@�j@�bN@�j@�bN@�Z@�bN@�I�@��@;d@�@
=@
=@
=@~��@~��@~@}?}@|j@|�@{ƨ@{��@{S�@{@z��@zn�@z-@yx�@x��@x�@w�;@w�P@w|�@wK�@w
=@vE�@u�T@u�-@u�@uO�@t��@t�/@tZ@t�@sƨ@s�@sS�@s33@r�@rn�@q�^@qX@q7L@p��@pQ�@o�@oK�@nȴ@n�+@n$�@m�T@m/@l�@l�@k��@k33@j��@j^5@i��@i�@h1'@g|�@g�@f�R@fE�@e�-@e?}@d�/@dz�@d�@cƨ@c�@c33@b��@b=q@bJ@a��@a�^@aX@`�`@`Ĝ@`�9@`r�@`A�@_��@_;d@_;d@_�@^�y@^ȴ@^��@^ff@]�-@]/@\�@\�j@[��@[�@[dZ@[@Z��@Z-@Y�^@YG�@Y%@X��@XbN@W��@Wl�@W;d@V��@Vȴ@V�R@V5?@U��@Up�@U/@UV@T�@T�/@T�j@T9X@Sƨ@St�@SS�@S33@So@R��@Rn�@R=q@R�@Q��@Qhs@Q7L@Q&�@Q�@Q%@P�`@P�u@Pb@P  @O�P@N�y@Nff@N$�@M�T@M�-@Mp�@M/@L��@L�@Lz�@Lj@LZ@K��@K�F@K��@Kt�@KdZ@K33@J�@J�!@JM�@J-@I�@I�^@IX@H�`@Hr�@H  @G��@F��@FV@FE�@F5?@F$�@E�@E@E�h@EV@D�j@D�j@D�@D��@D�D@D9X@C�F@CS�@B�@B��@Bn�@B=q@BJ@A��@A��@A�7@AX@A&�@A&�@A�@A%@@��@@Ĝ@@�u@@bN@@1'@@b@?�@?��@?��@?��@?�P@?;d@>�y@>�+@>E�@=�h@=V@<�j@<�j@<��@<�D@<j@<j@<�@;"�@:�H@:��@:�\@:^5@:-@9�@8�@8A�@8  @7�@7\)@6��@6��@6��@6��@6V@65?@5�T@5�@5/@4�/@4�@4j@3��@3�F@3"�@2~�@1��@1��@1��@1X@1�@0��@0�@01'@/�@/�@/K�@/�@.�@.��@.ff@.5?@.$�@.@-�T@-@-@-��@-?}@-�@,�/@,�D@,1@+��@+��@+S�@+o@*��@*M�@*-@)��@)x�@)x�@)x�@)X@)�@(�`@(Ĝ@(�@(b@'l�@';d@';d@&��@&v�@&5?@%�T@%�@%��@%p�@%O�@%O�@%/@$��@$�@$�/@$�@$j@$(�@$1@#��@#ƨ@#��@#��@#��@#��@#t�@"�@"n�@"=q@"J@!�@!��@!��@!G�@ ��@ �9@ bN@  �@   @��@�P@l�@l�@\)@K�@;d@
=@ȴ@�R@v�@E�@@�T@@�-@�@`B@?}@�@��@�D@z�@9X@��@�F@��@��@S�@"�@o@@�@�!@^5@�@��@��@�7@�7@x�@x�@x�@X@�@�@%@��@��@��@�`@Ĝ@Ĝ@�9@bN@bN@ �@K�@;d@+@�@��@��@�+@ff@V@{@@@@�@�T@��@��@�h@p�@�@�@��@��@z�@Z@(�@�@��@�F@��@33@@��@M�@=q@-@J@J@��@�@�#@��@x�@G�@G�@7L@%@�`@Ĝ@�9@��@��@�u@�@Q�@ �@�@�;@��@�@�@��@�+@ff@5?@$�@@��@`B@/@�@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B2-B2-B2-B2-B2-B2-B33B33B2-B33B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B1'B1'B+B#�B\BJ�Bo�B}�B�B�1B��B�-B�B�qB��B��B�B�HB��B��B��B��B�B�B�fB�B�B�5B�`B�TB�B�B�mB�yB�sB�`B�NB�/B��B��BŢB�!B��B�PB�B�Bq�B@�B)�B)�B�B+B
�B
�mB
�yB
ŢB
�XB
�LB
��B
�+B
`BB
F�B
>wB
VB
@�B
(�B
&�B
&�B
VB
�B
�B
1'B
�B
	7B	�`B	�B	��B	�-B	��B	��B	�1B	�uB	�{B	��B	�7B	iyB	r�B	s�B	gmB	Q�B	49B	�B	�B	"�B	oB	DB�B�yB�#B��B��B�B��B��BɺBɺB��BǮB�XB�?B�qBĜB�jB�?B�B��B�B��B��B��B��B��B�DB�PB�7B�B�oB�7B{�B~�Bx�BdZB`BBT�BcTBYB]/BaHBO�BYBhsBe`B^5B[#B^5BaHBgmBgmBaHBcTBbNBXBS�BH�B]/BYBZBYB`BBbNB`BB\)BR�BVB^5BbNBcTB`BB\)BR�BA�B=qBJ�B@�B@�BD�BD�BJ�BQ�BM�BN�BQ�BYB`BB`BB^5B^5BXBO�BS�BR�BS�BR�BG�BM�BO�B[#BVBZBbNBaHBYBZBYB[#B`BBbNBe`BffBr�Bu�Bn�By�B|�B}�Bz�Bz�By�B~�B�%B�%B�B�B� B�+B�%B�DB�DB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�FB�jB�}B��BBƨB��B��B��B��B��B��B�)B�;B�`B�yB�B�B�B�yB�B��B��B	B	+B	%B	B	1B	
=B	JB	VB	\B	oB	�B	{B	oB	�B	�B	 �B	"�B	$�B	&�B	.B	5?B	6FB	;dB	A�B	C�B	B�B	A�B	F�B	M�B	O�B	P�B	P�B	Q�B	VB	T�B	W
B	[#B	^5B	`BB	cTB	l�B	m�B	m�B	m�B	p�B	s�B	t�B	u�B	t�B	u�B	w�B	t�B	~�B	�B	�%B	�+B	�1B	�JB	�PB	�JB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�?B	�RB	�XB	�XB	�^B	�qB	�}B	�}B	��B	��B	ÖB	ȴB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�#B	�)B	�5B	�/B	�/B	�HB	�NB	�HB	�HB	�NB	�ZB	�`B	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�fB	�sB	�yB	�mB	�`B	�TB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
%B
+B
B
B
+B
1B
+B
%B
1B
DB
PB
PB
PB
PB
PB
PB
PB
PB
JB
JB
DB
\B
bB
hB
hB
bB
\B
oB
hB
hB
�B
�B
�B
�B
�B
{B
{B
uB
hB
uB
�B
�B
�B
�B
{B
uB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
!�B
 �B
 �B
"�B
#�B
"�B
#�B
#�B
$�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
-B
-B
.B
-B
-B
/B
/B
.B
.B
-B
/B
1'B
1'B
0!B
0!B
0!B
/B
.B
/B
1'B
1'B
0!B
1'B
33B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
33B
49B
6FB
5?B
6FB
6FB
49B
5?B
7LB
7LB
8RB
8RB
8RB
7LB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
9XB
;dB
<jB
<jB
;dB
;dB
:^B
:^B
;dB
:^B
:^B
;dB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
?}B
@�B
@�B
A�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
A�B
A�B
B�B
A�B
C�B
E�B
F�B
E�B
E�B
E�B
E�B
D�B
E�B
G�B
G�B
G�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
J�B
I�B
I�B
J�B
I�B
J�B
K�B
M�B
M�B
M�B
L�B
L�B
K�B
I�B
M�B
N�B
N�B
M�B
M�B
J�B
M�B
O�B
O�B
O�B
O�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
S�B
VB
W
B
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
YB
YB
YB
ZB
YB
YB
ZB
ZB
YB
YB
ZB
YB
YB
YB
YB
[#B
ZB
ZB
ZB
ZB
\)B
ZB
\)B
]/B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
ZB
\)B
]/B
[#B
]/B
]/B
]/B
_;B
^5B
]/B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
_;B
_;B
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
ffB
gmB
ffB
ffB
ffB
ffB
e`B
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
iyB
hsB
hsB
iyB
jB
jB
iyB
jB
k�B
k�B
jB
iyB
iyB
jB
jB
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
k�B
l�B
k�B
iyB
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
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
o�B
p�B
p�B
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
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
v�B
v�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B2GB2GB2GB2GB2GB2GB3MB3MB2GB3MB2GB2GB2GB2GB2GB2GB2GB2GB3MB3MB3MB3MB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB2GB1AB1[B1�B,qB'�B9BNBqvB� B��B�^B�5B��B�GB��B��BԯB�kB�TB��B��B��B�B�B��B�_B��B�iB��B�B�fB�B��B��B�B�yB�B�nB�5BԕB��B�zB��B�B�.B�%B�[BtBFtB-�B,�B�B
XB
�2B
�0B
�B
�XB
�6B
��B
��B
�)B
e`B
L~B
BAB
W�B
D�B
+�B
(�B
(�B
�B
�B
�B
2B
�B
�B	�KB	�B	ңB	�FB	��B	�FB	��B	��B	��B	��B	��B	m]B	t9B	t�B	iB	T{B	8RB	!bB	"B	$ZB	�B	�B�TB�qB��B�?B��B��BյBЗB��B�)BϫB�B��B�B�wB�B�qB�`B��B�yB�)B�eB�FB�B��B�B�"B�\B��B��B�[B�^B~(B�oBz�BhXBcBW�Bd�B[�B^�Bb�BR�BZ�Bi*BfLB_�B\�B_�Bb�Bh
Bh
Bb4Bc�Bb�BYeBU�BKB]�BZQB[=BZ�Ba-Bc:BaHB]~BT�BWYB_!Bb�Bc�B`�B\�BT{BD�B@iBL0BB�BBuBFtBFtBLJBR�BOBPBR�BY�B`\B`�B^�B^�BYBQhBUBTFBT�BS�BI�BOBQB[�BW?B[#Bb�BbBZkB[	BZ�B\]BabBcnBffBg�BshBvFBo�Bz^B}qB~wB{B{�Bz�B�B��B��B��B��B�B��B�+B�B�~B�uB�B�-B�VB�OB�:B�`B�>B�DB�zB��B��B��B��B��B�CB��B�B�B�'B�GB�EB�BB�HB�NB�NB͹B��BܒB��B��B��B��B��B�B�KB�9B�`B��B	�B	zB	�B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	[B	#B	!B	!-B	#:B	%`B	'mB	.cB	5ZB	6�B	;�B	A�B	C�B	B�B	BAB	GEB	N<B	PHB	QhB	QhB	RoB	VSB	UgB	WYB	[qB	^�B	`�B	c�B	l�B	m�B	m�B	m�B	p�B	tB	u?B	vB	uZB	v`B	xlB	u�B	cB	�{B	�tB	�_B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�KB	�kB	�wB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�'B	�3B	�B	�B	�KB	�7B	�0B	�6B	�<B	�B	�TB	�TB	�MB	�KB	�eB	׍B	׍B	�qB	�xB	ބB	�~B	ݲB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	� B	��B	�B	�!B	�-B	�B	�%B	�B	�B	�B	�2B	�LB	�$B	�>B	�0B	�B	�6B	�6B	�PB	�6B	�jB	�B	�]B
UB
AB
oB
uB
GB
{B
�B
�B
�B
zB
fB
fB
�B
�B
�B
�B
zB
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
�B
�B
B
	B
�B
�B
�B
�B
�B
B
�B
�B
B
B
B
B
B
)B
�B
B
B
/B
B
!B
!B
 �B
!-B
!B
 BB
#B
"4B
!HB
!HB
# B
$@B
#TB
$@B
$@B
%FB
'8B
'RB
'RB
'RB
(XB
)DB
)_B
*eB
+QB
+QB
+6B
+kB
+kB
-]B
-]B
.cB
-wB
-]B
/OB
/iB
.cB
.cB
-wB
/iB
1[B
1vB
0oB
0oB
0oB
/�B
.}B
/�B
1vB
1vB
0�B
1vB
3�B
2|B
2�B
2�B
2|B
3�B
3�B
3�B
4nB
3�B
4�B
6�B
5�B
6�B
6�G�O�B
5�B
7�B
7�B
8�B
8�B
8�B
7�B
6�B
7�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
9�B
;�B
<�B
<�B
;�B
;�B
:�B
:�B
;�B
:�B
:�B
;�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
?�B
@�B
@�B
A�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
A�B
A�B
B�B
BB
C�B
E�B
F�B
E�B
E�B
E�B
E�B
EB
E�B
G�B
G�B
G�B
F�B
F�B
FB
F�B
G�B
G�B
G�B
IB
IB
I�B
IB
J	B
J	B
J	B
J�B
J�B
J�B
J�B
J	B
J	B
KB
KB
KB
KB
LB
KB
K�B
LB
KB
J	B
J#B
J�B
J#B
KB
K�B
NB
N"B
NB
MB
MB
LJG�O�B
N"B
O(B
O(B
N"B
N<G�O�B
N<B
P.B
P.B
P.B
P.B
QB
R B
S&B
S&B
R:B
R:B
R B
R:B
R:B
R:B
R:B
R:B
RTB
S@B
RTB
S[B
TaB
V9B
WYB
VSB
V9B
VSB
VSB
WYB
WYB
W?B
WYB
X_B
X_B
YeB
YeB
YeB
Z7B
YeB
YKB
ZQB
ZQB
YeB
YeB
ZkB
YKB
YeB
YB
YB
[WB
ZkB
ZkB
Z�B
ZkB
\xG�O�B
\xB
]IB
\]B
\]B
\xB
\]B
\]B
[qB
[�B
Z�B
\xB
]dG�O�B
]dB
]~B
]~B
_pB
^�B
]~B
_pB
_VB
_pB
_�B
`\B
_�B
_�B
_�B
_pB
`�B
a�B
a�B
a�B
b�B
bhB
a|B
a�G�O�B
_�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d�B
d�B
d�B
e�B
f�B
g�B
f�B
f�B
f�B
f�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
h�B
h�B
i�B
j�B
j�B
i�B
j�B
k�B
k�B
j�B
i�B
i�B
j�B
j�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
k�B
l�B
k�G�O�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
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
pB
p�B
p�B
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
r�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
s�B
s�B
tB
tB
r�B
r�B
uB
t�B
u�B
vB
vB
u�B
vB
uB
u%B
v�B
v�B
xB
y	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111113111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111141111111111111111111111111111111111111111111111111111111411111111111141111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.36(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801090035092018010900350920180109003509201806221324262018062213242620180622132426201804050727432018040507274320180405072743  JA  ARFMdecpA19c                                                                20180105093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180105003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180105003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180105003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180105003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180105003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180105003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180105003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180105003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180105003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180105005522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180105153245  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180108153509  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180108153509  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180109000000  CF  PSAL_ADJUSTED_QCCB  D� G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222743  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042426  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                