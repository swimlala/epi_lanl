CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-02T03:38:55Z creation;2018-04-02T03:38:57Z conversion to V3.1;2019-12-23T06:24:24Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20180402033855  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  I2_0675_050                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Wƕ�> 1   @�W�H+�@6�~($x�b���a@1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D���D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  A��A&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D  D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%3D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�3DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�D���D��D�L�DÌ�D���D��D�L�DČ�D���D��D�L�DŌ�D���D��D�L�Dƌ�D���D��D�L�Dǌ�D���D��D�L�DȌ�D���D��D�L�DɌ�D���D��D�L�Dʌ�D���D��D�L�Dˌ�D���D��D�L�Ď�D���D��D�L�D͌�D���D��D�L�DΌ�D���D�	�D�L�Dό�D���D��D�L�DЌ�D���D��D�L�Dь�D���D��D�L�DҌ�D���D��D�L�Dӌ�D���D��D�L�DԌ�D���D��D�L�DՌ�D���D��D�L�D֌�D���D��D�L�D׌�D���D��D�L�D،�D���D��D�L�Dٌ�D���D��D�L�Dڌ�D���D��D�L�Dی�D���D��D�L�D܌�D���D��D�L�D݌�D���D��D�L�Dތ�D���D��D�L�Dߌ�D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�Q�A�-A��A��+A�p�A���A�M�A��A��!A�A�A�{A���A���A�33A�VA�?}A�v�A��\A�z�A���A���A��A��RA��\A�VA��A���A���A�XA�bA��A��A��/A��A��7A��A��A�|�A�x�A�\)A� �A�A���A��FA���A�-A��A���A�ffA���A�C�A���A�;dA���A���A�z�A�$�A�VA��A�$�A�%A���A�?}A�A���A��A�ZA� �A��yA���A�ffA�ƨA��A�7LA��mA�=qA��7A��A���A���A���A�jA�VA���A�`BA�~�A���A�5?A�O�A��A��
A�%A���A�XA��HA�ffA�S�A�7LA�5?A�bA��A|(�AzAx{Au%ApĜAm��Al��Al �Ai�AfjAdz�Ab�9A\~�AZ��AY�AW�^AUƨAS�AQ�PAPE�AO�FAN�/ANz�AL�AK��AK�AJĜAJv�AI`BAG�AF�+AE�AB��A@$�A?;dA>�jA>bA<�`A;��A;VA:I�A9�A8��A7A5hsA5�A3oA1&�A0{A.ffA-33A+\)A*A�A)?}A'��A'\)A&��A$��A#�wA#|�A"{A!��A ��A�7AffA�#A��AXA�A��A��A�TA%A5?A��A��A�#A~�A$�A��A�`AbNAdZAn�A�AE�A�A+A��A�A��AoA	�A	XA�yAbNA\)AK�Az�Ax�A �A��A�A�jAjA�FA ��A jA (�@���@��@��R@�M�@�x�@��9@���@�9X@���@�hs@���@�(�@�l�@�+@�h@�bN@��y@�7L@��m@�
=@�=q@�9X@�"�@�{@��#@���@�/@��;@�ȴ@�A�@�\)@�Ĝ@�$�@�~�@�v�@� �@�`B@��T@�Z@җ�@��@�\)@Η�@�Q�@�V@ǥ�@��@�Z@�l�@�M�@��@���@�I�@�|�@��@���@���@�5?@�z�@��
@���@��F@�C�@���@�M�@���@��@�z�@�Z@��
@�\)@�;d@�+@��@���@�X@��@��@���@��j@��/@��w@�33@�@�ff@���@���@��
@�;d@��@�~�@��^@���@��@���@�r�@��@���@���@�33@��!@�-@���@�p�@�7L@��@��9@�bN@�1'@��;@�dZ@�;d@�
=@���@���@��+@�^5@��@���@��h@�/@�V@��@��`@�Z@� �@��@�l�@�+@���@��R@�M�@�-@���@��T@��h@�?}@�7L@�/@��@�j@���@�33@��R@�-@��@��#@��7@�p�@�V@�V@�j@��@��@�C�@�
=@���@�~�@�{@���@��@��h@��@��@�j@�1'@��;@�l�@�S�@�33@��y@���@�n�@�V@�$�@��@���@��7@�p�@�G�@�V@��@�Z@� �@��@��m@��F@�l�@�dZ@�33@�+@�33@�33@�"�@��@��@�o@�
=@�o@���@��H@��H@�ȴ@���@�V@�5?@�$�@�@��@��T@��T@��#@���@�`B@���@�Ĝ@���@�Ĝ@��j@���@�1'@���@��F@��@�dZ@��@�
=@���@�@�p�@�p�@�?}@�?}@�&�@�Ĝ@�(�@� �@��@�b@�1@���@��m@��w@��@�t�@�C�@��y@�ȴ@���@��+@�ff@�$�@��T@���@�&�@���@��9@��u@�bN@�1'@��@���@��@��@�C�@�33@�
=@��H@���@�J@���@��^@�?}@���@��u@�bN@�I�@�A�@�A�@�9X@�1@���@���@��@�\)@�@��R@���@�V@�-@��@�J@�@���@�x�@�?}@�&�@���@��@�j@�A�@�(�@�  @��@|�@;d@
=@~�R@~�+@~$�@}@}`B@|��@|��@|�D@|(�@{t�@{33@{"�@z��@y�#@y�7@y%@xĜ@xQ�@x �@w�w@w+@w
=@v�@vff@u�@u@u`B@t�j@t9X@s�@sS�@r�\@r=q@q��@q��@q�#@qx�@q%@p�9@pr�@p �@o�@o|�@n�R@nv�@nE�@m@m�h@mp�@m`B@m?}@l�/@l�D@lI�@lI�@l(�@kƨ@kC�@k@j�@i��@i�7@i�@hr�@g�@g\)@g;d@g
=@f�R@f��@f��@f�+@f�+@fE�@f5?@f{@e@e��@ep�@d�@d��@d9X@c�m@c��@ct�@cdZ@c"�@b�H@b��@a�@ax�@a�@`�9@`�@`A�@`b@_�@_�@_\)@^�@^v�@^5?@^{@^@]�@]�-@]/@\�/@\�j@\��@\�D@\z�@\(�@[�
@[��@[�@[S�@["�@Z�!@Z^5@Z^5@ZM�@Z-@Y�@YX@Y&�@Y�@X��@XbN@W�;@W|�@W;d@V��@V�R@V{@U��@U@Up�@U/@T�D@T9X@S�
@S��@S�@SdZ@R�@R-@Q��@Q�7@PĜ@Pr�@PQ�@Pb@O�@O�;@O��@O�P@N��@N�R@N�+@Nv�@Nff@NV@N5?@N$�@M��@M�h@MV@Lz�@L9X@K��@Kƨ@K��@KdZ@KS�@K33@K"�@J��@J-@I��@I��@I&�@H��@HA�@G�@G�w@G|�@G+@F�y@F�@F�@F��@Fff@Fff@F$�@F{@E�@E�-@E�@E?}@D�@D�j@D��@Dz�@D�@C��@C�@C33@C@B�@B��@Bn�@Bn�@BM�@BJ@A��@AG�@A&�@A�@@�`@@�u@@�@@Q�@@1'@?�;@?l�@?+@>�y@>�R@>�+@>@=@=p�@<�@<z�@<(�@;�
@;�F@;�@;33@:�H@:�!@:��@:~�@9�#@9hs@9&�@8��@8�u@8A�@7�;@7�w@7K�@7�@6ȴ@6$�@6@5�T@5�-@5��@5`B@4�/@4��@49X@41@3�F@3t�@3"�@2��@2^5@1�@1hs@17L@1�@0��@0Ĝ@0�u@0�@0 �@/�;@/�P@/+@.��@.�y@.�R@.ff@.{@-�-@-`B@-�@-V@,�@,�@,I�@+�m@+ƨ@+33@*��@*^5@*J@)�@)�@)��@)X@)&�@(�`@(bN@( �@'�@'��@'�@'l�@&v�@&{@&@%�h@%p�@%?}@%�@%V@%V@$��@$�@$Z@$1@#�F@#��@#�@#S�@#o@"��@"�\@"^5@"J@!��@!��@!��@!G�@ Ĝ@ �9@ �9@ �u@ Q�@ 1'@   @�;@��@+@�@��@�R@ff@E�@{@��@�@?}@�j@�D@z�@j@Z@9X@�@�m@�F@t�@33@�H@��@��@~�@n�@n�@-@�@��@X@&�@%@%@�`@��@�@bN@Q�@A�@ �@  @�@��@�P@l�@\)@
=@�@�@��@@��@��@p�@`B@O�@�@��@�/@�@j@j@j@I�@9X@(�@(�@(�@1@��@��@��@�
@�F@dZ@C�@33@"�@�@��@n�@=q@�@J@��@��@�^@��@hs@7L@&�@�@��@Ĝ@�u@r�@A�@b@�@��@��@�P@�P@l�@+@��@�@ȴ@�R@��@�+@V@$�@@�@�-@p�@`B@O�@?}@�@�@�@�@V@��@�j@I�@1@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�Q�A�-A��A��+A�p�A���A�M�A��A��!A�A�A�{A���A���A�33A�VA�?}A�v�A��\A�z�A���A���A��A��RA��\A�VA��A���A���A�XA�bA��A��A��/A��A��7A��A��A�|�A�x�A�\)A� �A�A���A��FA���A�-A��A���A�ffA���A�C�A���A�;dA���A���A�z�A�$�A�VA��A�$�A�%A���A�?}A�A���A��A�ZA� �A��yA���A�ffA�ƨA��A�7LA��mA�=qA��7A��A���A���A���A�jA�VA���A�`BA�~�A���A�5?A�O�A��A��
A�%A���A�XA��HA�ffA�S�A�7LA�5?A�bA��A|(�AzAx{Au%ApĜAm��Al��Al �Ai�AfjAdz�Ab�9A\~�AZ��AY�AW�^AUƨAS�AQ�PAPE�AO�FAN�/ANz�AL�AK��AK�AJĜAJv�AI`BAG�AF�+AE�AB��A@$�A?;dA>�jA>bA<�`A;��A;VA:I�A9�A8��A7A5hsA5�A3oA1&�A0{A.ffA-33A+\)A*A�A)?}A'��A'\)A&��A$��A#�wA#|�A"{A!��A ��A�7AffA�#A��AXA�A��A��A�TA%A5?A��A��A�#A~�A$�A��A�`AbNAdZAn�A�AE�A�A+A��A�A��AoA	�A	XA�yAbNA\)AK�Az�Ax�A �A��A�A�jAjA�FA ��A jA (�@���@��@��R@�M�@�x�@��9@���@�9X@���@�hs@���@�(�@�l�@�+@�h@�bN@��y@�7L@��m@�
=@�=q@�9X@�"�@�{@��#@���@�/@��;@�ȴ@�A�@�\)@�Ĝ@�$�@�~�@�v�@� �@�`B@��T@�Z@җ�@��@�\)@Η�@�Q�@�V@ǥ�@��@�Z@�l�@�M�@��@���@�I�@�|�@��@���@���@�5?@�z�@��
@���@��F@�C�@���@�M�@���@��@�z�@�Z@��
@�\)@�;d@�+@��@���@�X@��@��@���@��j@��/@��w@�33@�@�ff@���@���@��
@�;d@��@�~�@��^@���@��@���@�r�@��@���@���@�33@��!@�-@���@�p�@�7L@��@��9@�bN@�1'@��;@�dZ@�;d@�
=@���@���@��+@�^5@��@���@��h@�/@�V@��@��`@�Z@� �@��@�l�@�+@���@��R@�M�@�-@���@��T@��h@�?}@�7L@�/@��@�j@���@�33@��R@�-@��@��#@��7@�p�@�V@�V@�j@��@��@�C�@�
=@���@�~�@�{@���@��@��h@��@��@�j@�1'@��;@�l�@�S�@�33@��y@���@�n�@�V@�$�@��@���@��7@�p�@�G�@�V@��@�Z@� �@��@��m@��F@�l�@�dZ@�33@�+@�33@�33@�"�@��@��@�o@�
=@�o@���@��H@��H@�ȴ@���@�V@�5?@�$�@�@��@��T@��T@��#@���@�`B@���@�Ĝ@���@�Ĝ@��j@���@�1'@���@��F@��@�dZ@��@�
=@���@�@�p�@�p�@�?}@�?}@�&�@�Ĝ@�(�@� �@��@�b@�1@���@��m@��w@��@�t�@�C�@��y@�ȴ@���@��+@�ff@�$�@��T@���@�&�@���@��9@��u@�bN@�1'@��@���@��@��@�C�@�33@�
=@��H@���@�J@���@��^@�?}@���@��u@�bN@�I�@�A�@�A�@�9X@�1@���@���@��@�\)@�@��R@���@�V@�-@��@�J@�@���@�x�@�?}@�&�@���@��@�j@�A�@�(�@�  @��@|�@;d@
=@~�R@~�+@~$�@}@}`B@|��@|��@|�D@|(�@{t�@{33@{"�@z��@y�#@y�7@y%@xĜ@xQ�@x �@w�w@w+@w
=@v�@vff@u�@u@u`B@t�j@t9X@s�@sS�@r�\@r=q@q��@q��@q�#@qx�@q%@p�9@pr�@p �@o�@o|�@n�R@nv�@nE�@m@m�h@mp�@m`B@m?}@l�/@l�D@lI�@lI�@l(�@kƨ@kC�@k@j�@i��@i�7@i�@hr�@g�@g\)@g;d@g
=@f�R@f��@f��@f�+@f�+@fE�@f5?@f{@e@e��@ep�@d�@d��@d9X@c�m@c��@ct�@cdZ@c"�@b�H@b��@a�@ax�@a�@`�9@`�@`A�@`b@_�@_�@_\)@^�@^v�@^5?@^{@^@]�@]�-@]/@\�/@\�j@\��@\�D@\z�@\(�@[�
@[��@[�@[S�@["�@Z�!@Z^5@Z^5@ZM�@Z-@Y�@YX@Y&�@Y�@X��@XbN@W�;@W|�@W;d@V��@V�R@V{@U��@U@Up�@U/@T�D@T9X@S�
@S��@S�@SdZ@R�@R-@Q��@Q�7@PĜ@Pr�@PQ�@Pb@O�@O�;@O��@O�P@N��@N�R@N�+@Nv�@Nff@NV@N5?@N$�@M��@M�h@MV@Lz�@L9X@K��@Kƨ@K��@KdZ@KS�@K33@K"�@J��@J-@I��@I��@I&�@H��@HA�@G�@G�w@G|�@G+@F�y@F�@F�@F��@Fff@Fff@F$�@F{@E�@E�-@E�@E?}@D�@D�j@D��@Dz�@D�@C��@C�@C33@C@B�@B��@Bn�@Bn�@BM�@BJ@A��@AG�@A&�@A�@@�`@@�u@@�@@Q�@@1'@?�;@?l�@?+@>�y@>�R@>�+@>@=@=p�@<�@<z�@<(�@;�
@;�F@;�@;33@:�H@:�!@:��@:~�@9�#@9hs@9&�@8��@8�u@8A�@7�;@7�w@7K�@7�@6ȴ@6$�@6@5�T@5�-@5��@5`B@4�/@4��@49X@41@3�F@3t�@3"�@2��@2^5@1�@1hs@17L@1�@0��@0Ĝ@0�u@0�@0 �@/�;@/�P@/+@.��@.�y@.�R@.ff@.{@-�-@-`B@-�@-V@,�@,�@,I�@+�m@+ƨ@+33@*��@*^5@*J@)�@)�@)��@)X@)&�@(�`@(bN@( �@'�@'��@'�@'l�@&v�@&{@&@%�h@%p�@%?}@%�@%V@%V@$��@$�@$Z@$1@#�F@#��@#�@#S�@#o@"��@"�\@"^5@"J@!��@!��@!��@!G�@ Ĝ@ �9@ �9@ �u@ Q�@ 1'@   @�;@��@+@�@��@�R@ff@E�@{@��@�@?}@�j@�D@z�@j@Z@9X@�@�m@�F@t�@33@�H@��@��@~�@n�@n�@-@�@��@X@&�@%@%@�`@��@�@bN@Q�@A�@ �@  @�@��@�P@l�@\)@
=@�@�@��@@��@��@p�@`B@O�@�@��@�/@�@j@j@j@I�@9X@(�@(�@(�@1@��@��@��@�
@�F@dZ@C�@33@"�@�@��@n�@=q@�@J@��@��@�^@��@hs@7L@&�@�@��@Ĝ@�u@r�@A�@b@�@��@��@�P@�P@l�@+@��@�@ȴ@�R@��@�+@V@$�@@�@�-@p�@`B@O�@?}@�@�@�@�@V@��@�j@I�@1@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�hB�hB�hB�oB�uB��B�^BÖBɺB�B�BB�fB�B�B��BBuB#�B,B<jBL�BT�BH�BB�BA�BE�BH�BK�BN�BS�B\)BhsBk�Bo�Bs�Bp�Bo�Bp�Br�Bt�Bv�Bu�Bv�Bw�B�B�1B��B��B��B��B��B��B�1B�B�B� B{�Bv�Bq�BjBdZBdZBcTBaHB`BB[#BS�BH�B5?B+B�B�BPB	7BBB��B�fB�#B�B��B�wB��B��B�hBq�BK�B33B&�B�B
�B
�
B
�-B
��B
�7B
n�B
cTB
VB
E�B
2-B
hB	�B	ɺB	�^B	�!B	��B	v�B	VB	M�B	H�B	6FB	 �B	bB		7B�yB�;B��BĜB�9B��B��B�PB�JB�7B�+B�Bv�Bu�Bs�Br�Bo�Bm�BgmBcTBYBQ�BK�BJ�BI�BH�BF�BD�BB�BA�BC�BE�B@�B=qB;dB8RB7LB5?B5?B9XB@�BD�BF�BD�BD�B?}B:^B8RB6FB7LB=qB=qB:^B9XB9XB<jB=qB<jB;dB>wB@�BA�B@�B:^B9XB33B2-B49B1'B0!B/B,B+B(�B'�B+B1'B33B33B6FB49B2-B/B.B2-B7LB8RB7LB1'B0!B2-B49B7LB9XB7LB:^B:^B:^B;dB;dB;dB<jB=qB:^B8RB49B33B2-B2-B2-B1'B0!B0!B0!B/B.B.B-B/B/B0!B0!B/B0!B1'B2-B5?B6FB8RB2-B9XB=qB>wB:^B@�BC�BE�BF�BF�BG�BD�BG�BE�BH�BJ�BK�BK�BK�BP�BS�BYB[#BZB[#B]/B_;BaHBcTBhsBjBl�Bm�Bp�Bu�Bu�Bu�Bu�Bu�Bw�By�By�B{�Bz�B{�B|�B}�B�B�1B�JB�oB�{B��B��B��B�B�B�-B�?B�XB�wB�}B��BBǮBɺB��B��B��B��B��B��B�
B�B�B�)B�/B�;B�NB�TB�ZB�ZB�`B�mB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	1B		7B	DB	DB	VB	bB	oB	{B	�B	�B	 �B	"�B	%�B	+B	,B	.B	0!B	1'B	2-B	49B	6FB	9XB	;dB	=qB	>wB	?}B	C�B	I�B	M�B	Q�B	S�B	XB	]/B	^5B	`BB	cTB	e`B	iyB	l�B	n�B	q�B	t�B	v�B	w�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�DB	�JB	�PB	�PB	�PB	�VB	�VB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�FB	�RB	�XB	�dB	�dB	�dB	�^B	�XB	�^B	�jB	�qB	��B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�TB	�TB	�TB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
	7B

=B
DB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
oB
hB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
49B
49B
5?B
5?B
5?B
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
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
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
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
W
B
W
B
W
B
W
B
W
B
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
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
gmB
gmB
gmB
gmB
gmB
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
iyB
iyB
iyB
iyB
iyB
iyB
hsB
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
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�4B�NB�4B�:B�[B��B�*B�{BɠB��B�'B�LB�wB�B��B�B[B#�B+�B<PBL�BT�BH�BBuBAoBE�BH�BK�BN�BS�B[�Bh>BkQBo�Bs�Bp�Bo�Bp�Br�Bt�Bv�Bu�Bv�Bw�B��B�B�gB�SB�_B��B��B��B��B�B��B�B{�Bv�Bq�BjeBd@Bd@Bc Ba-B`'B[	BS�BH�B5B*�B�BSBB	B�B �B��B�LB��B��B��B�]B��B��B�NBqvBK�B3B&�BYB
�B
��B
�B
�MB
�B
ncB
c:B
U�B
EmB
1�B
4B	�WB	ɆB	�DB	�B	�B	v�B	U�B	M�B	H�B	6+B	 �B	.B		B�DB�!BбBāB�B��B�mB�B�0B�B�B��Bv�Bu�Bs�Br|BoiBm]Bg8Bc:BX�BQ�BK�BJ�BI�BH�BFtBDgBB[BAUBC{BE�B@iB=<B;0B8B72B5B5B9>B@OBDgBFtBDgBDgB?HB:*B8B6B7B=<B=<B:*B9$B9>B<6B=VB<6B;JB>BB@OBAUB@iB:*B9$B2�B1�B4B0�B/�B.�B+�B*�B(�B'�B*�B1B2�B2�B6B4B2B.�B-�B1�B7B8B7B0�B/�B1�B4B7B9$B7B:*B:DB:*B;0B;0B;0B<6B=VB:*B8B4B3B1�B2B1�B0�B/�B/�B/�B.�B-�B-�B,�B.�B.�B/�B/�B/ B/�B0�B1�B5B6B8B2B9$B=<B>BB:DB@OBCaBEmBF�BFtBGzBDgBGzBEmBH�BJ�BK�BK�BK�BP�BS�BX�BZ�BZBZ�B\�B_BaBc Bh>BjKBlWBm]BpoBu�Bu�Bu�Bu�ButBw�By�By�B{�Bz�B{�B|�B}�B��B��B�B�:B�FB�kB��B��B��B��B��B�B�$B�BB�.B�OB�[B�zBɆBʌB̘BΥBбB��B��B��B��B��B��B��B�B�B� B�&B�&B�,B�8B�>B�KB�WB�CB�cB�cB�iB�oB�B��B��B��B��B��B��B	�B	�B	�B	�B	�B		B	)B	B	"B	.B	:B	FB	eB	�B	 �B	"�B	%�B	*�B	+�B	-�B	/�B	0�B	1�B	4B	6B	9$B	;0B	=<B	>BB	?HB	CaB	I�B	M�B	Q�B	S�B	W�B	\�B	^B	`B	c B	e,B	iDB	lWB	ncB	qvB	t�B	v�B	w�B	x�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�<B	�(B	�.B	�,B	�MB	�SB	�YB	�eB	�kB	�kB	�qB	�xB	��B	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�0B	�0B	�0B	�*B	�$B	�*B	�6B	�<B	�OB	�[B	�tB	ȀB	ɠB	ɆB	˒B	˒B	˒B	̘B	ΥB	ΥB	ϫB	бB	бB	ѷB	ѷB	ѷB	ѷB	ЗB	ѷB	��B	��B	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	� B	� B	�2B	�8B	�>B	�DB	�KB	�KB	�KB	�QB	�QB	�WB	�WB	�]B	�]B	�]B	�cB	�cB	��B	�oB	�oB	�UB	�oB	�vB	�|B	�B	�nB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

	B
B
B
B
B
B
B
"B
"B
"B
(B
(B
(B
BB
BB
.B
.B
.B
.B
4B
4B
NB
4B
4B
NB
4B
TB
4B
:B
:B
@B
@B
MB
MB
MB
MB
MB
SB
mB
YB
YB
YB
YB
?B
YB
YB
YB
?B
YB
_B
_B
_B
eB
B
eB
KB
kB
kB
eB
kB
kB
kB
qB
qB
WB
]B
]B
xB
~B
~B
~B
dB
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
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/ B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
4B
4B
4B
5B
5B
5%B
6B
6B
6+B
7B
6�B
7B
7B
7B
7B
7B
7B
7B
6�B
8B
8B
8B
8B
8B
8B
9>B
9$B
9$B
:*B
:DB
:DB
:*B
;0B
;0B
;0B
;0B
;0B
<6B
<6B
<PB
<6B
=<B
=<B
=<B
=<B
=<B
=VB
="B
=<B
>BB
>BB
>BB
?cB
?HB
?HB
@OB
@iB
AUB
AUB
AoB
AoB
AUB
B[B
B[B
B[B
B[B
CaB
CaB
CaB
DgB
DgB
DMB
DgB
DgB
EmB
EmB
EmB
F�B
G�B
GzB
GzB
GzB
GzB
H�B
H�B
HfB
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
^�B
_B
_!B
_B
_B
_B
^�B
`B
`B
`B
`B
aB
aB
aB
`�B
aB
aB
bB
bB
bB
a�B
bB
c:B
c B
c B
c B
c B
cB
d&B
d&B
d&B
d&B
d&B
d&B
e,B
e,B
d&B
eB
e,B
e,B
fLB
f2B
f2B
f2B
f2B
f2B
fB
gRB
g8B
g8B
g8B
g8B
g8B
gB
h>B
g8B
h$B
h>B
h>B
h>B
h>B
h>B
iDB
iDB
iDB
i*B
iDB
iDB
hXB
hXB
iDB
iDB
iDB
iDB
iDB
iDB
iDB
jKB
jKB
jKB
kQB
kkB
k6B
kQB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
m]B
m]B
m]B
mwB
m]B
mwB
m]B
m]B
ncB
n}B
ncB
ncB
ncB
n}B
ncB
oOB
oiB
oiB
oiB
oiB
oOB
oiB
oiB
poB
pUG�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.4(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804060045552018040600455520180406004555201804261731392018042617313920180426173139JA  ARFMdecpA19c                                                                20180402123527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180402033855  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180402033855  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180402033856  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180402033857  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180402033857  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180402033857  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180402033857  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180402033857  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180402033857                      G�O�G�O�G�O�                JA  ARUP                                                                        20180402035754                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180401154210  CV  JULD            G�O�G�O�F¾5                JM  ARSQJMQC2.0                                                                 20180402000000  CF  PSAL_ADJUSTED_QCD�  D�  G�O�                JM  ARCAJMQC2.0                                                                 20180405154555  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180405154555  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426083139  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                