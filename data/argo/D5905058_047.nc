CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-19T09:35:35Z creation;2018-03-19T09:35:37Z conversion to V3.1;2019-12-23T06:25:11Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180319093535  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA  I2_0675_047                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�T�~ܺ�1   @�T�5� @6�(����b�6z�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BJ  BR  BY��Ba��Bi��Bq��By��B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>� C@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCt� CvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!  D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�L�D���D���D��D�L�D���D���D� D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D�ɚD��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D�	�D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�D���D��D�L�DÌ�D���D��D�L�DČ�D���D��D�L�DŌ�D���D��D�L�Dƌ�D���D��D�L�Dǌ�D���D��D�L�DȌ�D���D��D�L�DɌ�D���D��D�L�Dʌ�D���D��D�L�Dˌ�D���D��D�L�Ď�D���D��D�L�D͌�D���D��D�L�DΌ�D���D��D�L�Dό�D���D��D�L�DЌ�D���D��D�L�Dь�D���D��D�L�DҌ�D���D��D�L�Dӌ�D���D��D�L�DԌ�D���D��D�L�DՌ�D���D��D�L�D֌�D���D��D�L�D׌�D���D��D�L�D،�D���D��D�L�Dٌ�D���D��D�L�Dڌ�D���D��D�L�Dی�D���D��D�L�D܌�D���D��D�L�D݌�D���D��D�L�Dތ�D���D��D�L�Dߌ�D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�P D��D���D��D�L�D��D�� D� D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�P D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A�33A�5?A�7LA�7LA�9XA�;dA�=qA�?}A�A�A�A�A�A�A�C�A�C�A�?}A�9XA��A���A��`A��A��PA��7A��A�|�A�z�A�x�A�v�A�r�A�dZA�"�A���A��`A��^A�K�A��A��A���A�"�A���A��9A���A�1'A��
A��A�9XA��7A��A�"�A�  A���A��A�p�A�=qA���A���A���A��uA�33A�Q�A�E�A��A��A�VA�;dA���A��A���A� �A���A��\A���A���A���A�^5A�$�A��^A��A�C�A��
A��A��A�A�K�A��PA�ffA��RA�oA���A�dZA��A�n�A�9XA��A�;dA��A���A�33A���A�`BA��A�~�A�JA��hA�E�A���A��jA��
A�$�A�`BA��RA�A|�`Az��Aw�
ArȴAnv�Ak�
Ai�FAeK�AbbNA_XA[�wA[AYƨAWƨAV1'AM�-AJ�!AI�-AH-AF1'AE�^AE\)AD��ADffAD�AC�7A@^5A=�7A:��A9��A8Q�A6 �A2ffA0Q�A/��A.�`A.bNA-��A-l�A-C�A-+A,�`A,��A,{A+x�A)�^A(r�A'��A&�A&�!A&��A&jA&E�A%�A%�7A%33A%�A$��A$�DA#�TA#p�A#33A!��A bNA��A�yA��AA�A��A�-A��A�
Az�A�Al�AjA
=Ap�A��AbA��A
��A	��A��AJA�mAAK�A��An�A��Ap�AO�A�A�\A�-A�A�A�#A�A =q@��w@�E�@��P@�r�@���@�ƨ@�R@�X@� �@�n�@�j@�|�@�
=@�&�@�`B@���@�ff@���@۾w@���@�@���@�1@׾w@�
=@�?}@Ӯ@�-@��m@υ@�M�@̓u@�|�@�G�@ǝ�@Ɵ�@��@őh@�O�@Ĵ9@þw@�l�@���@�V@�G�@��/@�j@�  @���@��T@�hs@�I�@��@�~�@�p�@�X@�`B@�p�@��`@�o@��@��-@�%@���@���@��@���@�hs@��7@�`B@�O�@�/@�%@��@�ƨ@�o@���@�@��h@�r�@���@���@���@�K�@�o@���@�V@��T@�x�@�V@��j@�I�@�  @��@�C�@�ff@��@�J@�@���@��@��7@���@��j@�  @�t�@��@�@��@�@���@�J@�x�@�/@���@�I�@�  @�ƨ@�S�@�
=@���@�v�@�J@��7@�/@��@�b@��@�;d@��@��@�@�C�@��!@�E�@�$�@���@��7@��@�Z@� �@�b@��
@��@�t�@�\)@�C�@�33@�o@���@��!@�~�@�^5@�5?@��T@��7@�O�@�V@��@��9@��D@�Q�@���@���@��F@���@��P@�t�@�33@��H@�ȴ@���@�~�@�n�@�J@�@��@�`B@��@�p�@�X@�?}@��@�Ĝ@��D@�r�@�Q�@�(�@�1@��;@��w@��F@��@��P@�\)@�
=@���@�~�@�=q@��@��^@��@�X@�7L@��@��@���@���@��@�Q�@�  @��m@��
@��F@���@�t�@�+@�ȴ@�ff@�^5@�V@�5?@�-@�J@���@��^@��@�?}@��/@��9@�z�@�9X@��@�ƨ@��@���@�C�@�ȴ@�~�@�ff@�=q@�{@��@���@��-@�x�@�?}@��@���@��9@��u@�j@�I�@�(�@��m@�ƨ@��P@�\)@�33@�@��R@�~�@�ff@�^5@�E�@�{@��T@��h@�X@�O�@�G�@�/@��@���@�j@�A�@�1@��@�ƨ@���@�C�@�
=@���@�ff@�@��#@��T@���@��@�/@���@��D@�Z@�(�@�b@�  @��@�@~�R@~$�@}��@}?}@|�@|I�@{�
@{"�@z��@z��@zn�@zJ@y��@y��@y�@xbN@xb@w�@w\)@vȴ@v�+@vV@u�@u@u`B@uO�@u?}@u/@tz�@t�@sƨ@st�@sdZ@s33@r��@r��@r=q@q��@qX@p��@p��@pA�@p �@pb@o�@o��@o�@o|�@n��@nff@m�T@m�@m/@mV@l�/@l�@l�@k��@k�F@k�@kS�@j�H@jM�@i��@i��@iX@i%@h�u@h �@g�;@g�@g�w@g�P@f�@fv�@e��@e�h@e`B@e�@d��@d9X@cƨ@c��@cdZ@co@b��@b-@a�@ahs@a%@`�9@`�u@`bN@`1'@_��@_�@^�@^V@]��@]p�@\��@\��@\�j@\j@\j@\9X@[ƨ@[@Z��@Z^5@Y��@Y��@Y��@Yx�@Yhs@Y&�@X��@X�`@X��@Xr�@X �@W�@Wl�@V�y@Vȴ@V��@Vff@VE�@V{@U@UO�@T��@Tz�@Tj@S�m@St�@SS�@SC�@R��@Q��@QX@Q�@P�9@PA�@P  @O��@O|�@OK�@O;d@Nȴ@N$�@N{@M�T@M�T@M�T@Mp�@L�/@L�j@L�@L�@L�j@L�@L�D@L�D@Lj@Kƨ@KdZ@K@J�!@J�@I�#@I�@I�#@I�#@Ix�@Ihs@IX@IG�@I%@H�`@H��@H��@H�u@Hr�@HQ�@H  @G�w@GK�@F��@F�@Fȴ@F��@F�+@FV@E�@Ep�@E?}@E/@D�/@D�D@DI�@D�@C�m@C��@CdZ@B��@B�!@B~�@B-@A�^@A&�@@Ĝ@@r�@@A�@@ �@@  @?�@?�P@?+@?�@?
=@?
=@>��@>�@>��@>$�@=@=��@=�@=/@<�/@<�@<�D@<(�@;�
@;ƨ@;�@;33@:��@:^5@:M�@:=q@:-@9��@9x�@9G�@8Ĝ@8�u@8 �@7�;@7�w@7�P@7+@7
=@6ȴ@6V@6$�@6{@6@5@5p�@5V@4��@4Z@4�@3�
@3ƨ@3�F@3dZ@3"�@2��@2n�@2^5@2M�@2�@1�@1��@1x�@17L@0��@0��@0A�@0 �@0b@/�@/�P@/\)@/+@.ȴ@.�+@.5?@.$�@.{@-��@-�h@-�@-p�@-O�@-/@,�/@,��@,�@,�@,��@,z�@,Z@,(�@+��@+o@*��@*�\@*n�@*-@)�@)��@)X@)&�@(��@(b@'�@'��@'|�@'K�@'�@&�y@&�y@&�@&�R@&ff@%@%�-@%��@%p�@%O�@%/@$j@$1@#�m@#�
@#�F@#��@#�@#S�@#o@"�H@"��@"�!@"~�@"J@!�7@ ��@ �@ A�@  �@�@��@K�@
=@ȴ@ff@$�@�@��@p�@V@�j@�D@I�@�@�m@ƨ@dZ@C�@o@@�H@�!@��@~�@M�@J@�#@��@�7@G�@%@��@��@Ĝ@��@r�@A�@b@  @�;@�w@�P@\)@;d@�@��@E�@5?@$�@@��@�@O�@�@V@��@�j@z�@Z@I�@�@��@�@t�@S�@C�@C�@C�@S�@S�@33@o@@��@~�@M�@=q@�@J@J@�@��@�^@�7@G�@&�@%@Ĝ@r�@ �@�@�w@�P@l�@+@��@�@��@v�@ff@5?@@�T@��@��@��@`B@/@�@j@��@�F@S�@
��@
�!@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A�33A�5?A�7LA�7LA�9XA�;dA�=qA�?}A�A�A�A�A�A�A�C�A�C�A�?}A�9XA��A���A��`A��A��PA��7A��A�|�A�z�A�x�A�v�A�r�A�dZA�"�A���A��`A��^A�K�A��A��A���A�"�A���A��9A���A�1'A��
A��A�9XA��7A��A�"�A�  A���A��A�p�A�=qA���A���A���A��uA�33A�Q�A�E�A��A��A�VA�;dA���A��A���A� �A���A��\A���A���A���A�^5A�$�A��^A��A�C�A��
A��A��A�A�K�A��PA�ffA��RA�oA���A�dZA��A�n�A�9XA��A�;dA��A���A�33A���A�`BA��A�~�A�JA��hA�E�A���A��jA��
A�$�A�`BA��RA�A|�`Az��Aw�
ArȴAnv�Ak�
Ai�FAeK�AbbNA_XA[�wA[AYƨAWƨAV1'AM�-AJ�!AI�-AH-AF1'AE�^AE\)AD��ADffAD�AC�7A@^5A=�7A:��A9��A8Q�A6 �A2ffA0Q�A/��A.�`A.bNA-��A-l�A-C�A-+A,�`A,��A,{A+x�A)�^A(r�A'��A&�A&�!A&��A&jA&E�A%�A%�7A%33A%�A$��A$�DA#�TA#p�A#33A!��A bNA��A�yA��AA�A��A�-A��A�
Az�A�Al�AjA
=Ap�A��AbA��A
��A	��A��AJA�mAAK�A��An�A��Ap�AO�A�A�\A�-A�A�A�#A�A =q@��w@�E�@��P@�r�@���@�ƨ@�R@�X@� �@�n�@�j@�|�@�
=@�&�@�`B@���@�ff@���@۾w@���@�@���@�1@׾w@�
=@�?}@Ӯ@�-@��m@υ@�M�@̓u@�|�@�G�@ǝ�@Ɵ�@��@őh@�O�@Ĵ9@þw@�l�@���@�V@�G�@��/@�j@�  @���@��T@�hs@�I�@��@�~�@�p�@�X@�`B@�p�@��`@�o@��@��-@�%@���@���@��@���@�hs@��7@�`B@�O�@�/@�%@��@�ƨ@�o@���@�@��h@�r�@���@���@���@�K�@�o@���@�V@��T@�x�@�V@��j@�I�@�  @��@�C�@�ff@��@�J@�@���@��@��7@���@��j@�  @�t�@��@�@��@�@���@�J@�x�@�/@���@�I�@�  @�ƨ@�S�@�
=@���@�v�@�J@��7@�/@��@�b@��@�;d@��@��@�@�C�@��!@�E�@�$�@���@��7@��@�Z@� �@�b@��
@��@�t�@�\)@�C�@�33@�o@���@��!@�~�@�^5@�5?@��T@��7@�O�@�V@��@��9@��D@�Q�@���@���@��F@���@��P@�t�@�33@��H@�ȴ@���@�~�@�n�@�J@�@��@�`B@��@�p�@�X@�?}@��@�Ĝ@��D@�r�@�Q�@�(�@�1@��;@��w@��F@��@��P@�\)@�
=@���@�~�@�=q@��@��^@��@�X@�7L@��@��@���@���@��@�Q�@�  @��m@��
@��F@���@�t�@�+@�ȴ@�ff@�^5@�V@�5?@�-@�J@���@��^@��@�?}@��/@��9@�z�@�9X@��@�ƨ@��@���@�C�@�ȴ@�~�@�ff@�=q@�{@��@���@��-@�x�@�?}@��@���@��9@��u@�j@�I�@�(�@��m@�ƨ@��P@�\)@�33@�@��R@�~�@�ff@�^5@�E�@�{@��T@��h@�X@�O�@�G�@�/@��@���@�j@�A�@�1@��@�ƨ@���@�C�@�
=@���@�ff@�@��#@��T@���@��@�/@���@��D@�Z@�(�@�b@�  @��@�@~�R@~$�@}��@}?}@|�@|I�@{�
@{"�@z��@z��@zn�@zJ@y��@y��@y�@xbN@xb@w�@w\)@vȴ@v�+@vV@u�@u@u`B@uO�@u?}@u/@tz�@t�@sƨ@st�@sdZ@s33@r��@r��@r=q@q��@qX@p��@p��@pA�@p �@pb@o�@o��@o�@o|�@n��@nff@m�T@m�@m/@mV@l�/@l�@l�@k��@k�F@k�@kS�@j�H@jM�@i��@i��@iX@i%@h�u@h �@g�;@g�@g�w@g�P@f�@fv�@e��@e�h@e`B@e�@d��@d9X@cƨ@c��@cdZ@co@b��@b-@a�@ahs@a%@`�9@`�u@`bN@`1'@_��@_�@^�@^V@]��@]p�@\��@\��@\�j@\j@\j@\9X@[ƨ@[@Z��@Z^5@Y��@Y��@Y��@Yx�@Yhs@Y&�@X��@X�`@X��@Xr�@X �@W�@Wl�@V�y@Vȴ@V��@Vff@VE�@V{@U@UO�@T��@Tz�@Tj@S�m@St�@SS�@SC�@R��@Q��@QX@Q�@P�9@PA�@P  @O��@O|�@OK�@O;d@Nȴ@N$�@N{@M�T@M�T@M�T@Mp�@L�/@L�j@L�@L�@L�j@L�@L�D@L�D@Lj@Kƨ@KdZ@K@J�!@J�@I�#@I�@I�#@I�#@Ix�@Ihs@IX@IG�@I%@H�`@H��@H��@H�u@Hr�@HQ�@H  @G�w@GK�@F��@F�@Fȴ@F��@F�+@FV@E�@Ep�@E?}@E/@D�/@D�D@DI�@D�@C�m@C��@CdZ@B��@B�!@B~�@B-@A�^@A&�@@Ĝ@@r�@@A�@@ �@@  @?�@?�P@?+@?�@?
=@?
=@>��@>�@>��@>$�@=@=��@=�@=/@<�/@<�@<�D@<(�@;�
@;ƨ@;�@;33@:��@:^5@:M�@:=q@:-@9��@9x�@9G�@8Ĝ@8�u@8 �@7�;@7�w@7�P@7+@7
=@6ȴ@6V@6$�@6{@6@5@5p�@5V@4��@4Z@4�@3�
@3ƨ@3�F@3dZ@3"�@2��@2n�@2^5@2M�@2�@1�@1��@1x�@17L@0��@0��@0A�@0 �@0b@/�@/�P@/\)@/+@.ȴ@.�+@.5?@.$�@.{@-��@-�h@-�@-p�@-O�@-/@,�/@,��@,�@,�@,��@,z�@,Z@,(�@+��@+o@*��@*�\@*n�@*-@)�@)��@)X@)&�@(��@(b@'�@'��@'|�@'K�@'�@&�y@&�y@&�@&�R@&ff@%@%�-@%��@%p�@%O�@%/@$j@$1@#�m@#�
@#�F@#��@#�@#S�@#o@"�H@"��@"�!@"~�@"J@!�7@ ��@ �@ A�@  �@�@��@K�@
=@ȴ@ff@$�@�@��@p�@V@�j@�D@I�@�@�m@ƨ@dZ@C�@o@@�H@�!@��@~�@M�@J@�#@��@�7@G�@%@��@��@Ĝ@��@r�@A�@b@  @�;@�w@�P@\)@;d@�@��@E�@5?@$�@@��@�@O�@�@V@��@�j@z�@Z@I�@�@��@�@t�@S�@C�@C�@C�@S�@S�@33@o@@��@~�@M�@=q@�@J@J@�@��@�^@�7@G�@&�@%@Ĝ@r�@ �@�@�w@�P@l�@+@��@�@��@v�@ff@5?@@�T@��@��@��@`B@/@�@j@��@�F@S�@
��@
�!@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�XBĜB��B��B�/B�;B�;B�BB�HB�HB�HB�NB�TB�mBBoB�B,BYBffBffBm�BjBiyBhsBO�BPB�B�BPB�BoB%BBPB��B�B�B��B�5B�NB�mB�mB��B�B#�B&�B+B/B.B,B'�B�B�B{B\BVBPBJB	7B+BBB��B�B�yB�HB��B��B�jB�3B��B��B�JBt�Be`B9XB�BJB
��B
�TB
�B
ȴB
�!B
�bB
e`B
aHB
ZB
W
B
O�B
C�B
5?B
,B
�B
bB	�B	��B	�?B	��B	p�B	L�B	49B	"�B	1B�B�5B��B��BƨB�dB�-B�1Bu�Bq�Bn�BcTBaHB`BB_;B^5B^5B[#BO�B?}B49B.B,B)�B=qB6FB5?B:^BJ�BP�BO�BN�BN�BN�BN�BP�BT�BW
BVBT�BVBVBT�BT�BS�BR�BQ�BP�BP�BN�BN�BM�BL�BK�BK�BG�BF�BG�BH�BF�BH�BQ�BM�BG�B@�B=qB:^B5?B33B-B+B'�B(�B#�B"�B$�B'�B+B.B2-B33B2-B49B33B33B33B49B2-B/B.B,B,B+B)�B)�B)�B+B)�B+B)�B+B+B,B-B.B-B.B2-B2-B5?B1'B33B49B5?B5?B49B5?B:^B?}B>wB>wB;dB:^B:^B7LB6FB49B33B49B5?B5?B5?B7LB8RB9XB:^B;dB=qB>wB@�BB�BF�BG�BH�BK�BO�BW
BXBYB\)B_;BcTBbNB^5B^5BaHBdZBjBm�Bq�Bv�B|�B}�B|�B}�B}�B� B�B�B�B�+B�DB��B��B��B��B��B��B�B�B�!B�'B�-B�-B�FB�RB�dB�wBBŢBŢBƨBȴB��B��B��B��B��B��B�
B�B�)B�;B�HB�HB�`B�yB�B�B�B�B��B��B��B��B��B	  B	B	B	B	+B		7B	
=B	VB	\B	oB	�B	�B	�B	 �B	 �B	!�B	#�B	$�B	$�B	'�B	(�B	,B	-B	.B	/B	1'B	49B	5?B	7LB	8RB	:^B	>wB	B�B	E�B	H�B	I�B	K�B	N�B	Q�B	T�B	W
B	YB	YB	ZB	]/B	`BB	cTB	dZB	gmB	hsB	jB	m�B	m�B	p�B	s�B	t�B	v�B	x�B	x�B	z�B	}�B	� B	� B	�B	�B	�%B	�1B	�7B	�=B	�=B	�DB	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�?B	�?B	�FB	�RB	�RB	�^B	�jB	�wB	��B	��B	B	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
+B
1B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
uB
uB
uB
uB
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
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
+B
+B
+B
+B
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
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
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
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
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
J�B
I�B
I�B
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
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
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
W
B
W
B
XB
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
YB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
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
cTB
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
e`B
ffB
ffB
e`B
e`B
e`B
e`B
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
gmB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
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
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�+B�>B�gBѷB��B�B�B�!B�'B�-B�-B�-B�B� B�RB�BTB�B+�BX�Bf2BfLBmwBjeBiDBhXBO�B6B�B�B6BBTBB �BB��B�qB��BбB�B�4B�RB�RB��BxB#�B&�B*�B/ B-�B+�B'�B�ByBaBBB<B6B0B	BB�B �B��B�|B�_B�BϫB�iB�PB�B��B��B�0Bt�BeFB9>B�B0B
�B
�:B
��B
ȀB
�B
�.B
eFB
aB
Y�B
V�B
O�B
CaB
5B
+�B
�B
.B	�cB	��B	�%B	��B	poB	L�B	4B	"�B	B�iB�B͹B̳B�tB�0B��B��Bu�Bq�BncBc BaB`B_B^B^B[	BO�B?cB4B-�B+�B)�B=<B6B5B:DBJ�BP�BO�BN�BN�BN�BN�BP�BT�BV�BU�BT�BU�BU�BT�BT�BS�BR�BQ�BP�BP�BN�BN�BM�BL�BK�BK�BG�BFtBG�BH�BF�BH�BQ�BM�BGzB@iB=<B:*B5%B2�B,�B*�B'�B(�B#�B"�B$�B'�B*�B-�B1�B2�B1�B4B3B2�B2�B4B1�B/ B-�B+�B+�B*�B)�B)�B)�B*�B)�B*�B)�B*�B*�B+�B,�B-�B,�B-�B2B1�B5B0�B2�B4B5B5B4B5B:DB?HB>BB>BB;0B:*B:*B7B6B4B3B4B5B5B5B72B8B9$B:DB;0B=<B>BB@OBB[BFtBGzBH�BK�BO�BV�BW�BX�B[�B_Bc BbB^B^BaBd&BjKBm]BqvBv�B|�B}�B|�B}�B}�B�B��B��B��B��B�B�SB�kB�~B��B��B��B��B��B��B��B��B��B�B�B�0B�(B�[B�mB�mB�tBȀBʌB˒BϫBϫB��B��B��B��B��B�B�B�B�,B�DB�oB�vB�vB�B��B��B��B��B��B��B	�B	�B	B	�B		B	
	B	"B	(B	:B	YB	qB	�B	 �B	 �B	!�B	#�B	$�B	$�B	'�B	(�B	+�B	,�B	-�B	.�B	0�B	4B	5B	7B	8B	:*B	>BB	B[B	EmB	H�B	I�B	K�B	N�B	Q�B	T�B	V�B	X�B	X�B	Y�B	\�B	`B	c B	d&B	g8B	hXB	jKB	m]B	mCB	poB	s�B	t�B	v�B	x�B	x�B	z�B	}�B	�B	�B	��B	��B	��B	��B	�B	�	B	�	B	�)B	�B	�B	�FB	�SB	�_B	�qB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�8B	�*B	�6B	�BB	�OB	�UB	�[B	�gB	�mB	�mB	�tB	ȀB	ʌB	̘B	̘B	͟B	ΥB	ϫB	��B	бB	ҽB	��B	��B	��B	ּB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�2B	�2B	�LB	�8B	�>B	�KB	�QB	�kB	�]B	�]B	�]B	�cB	�iB	�oB	�iB	�oB	�oB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
	B
	B

	B

	B
B
B
B
B
B
B
B
"B
"B
"B
BB
(B
(B
.B
.B
.B
NB
NB
:B
:B
@B
@B
@B
@B
@B
@B
,B
aB
aB
FB
@B
@B
@B
@B
FB
,B
FB
MB
MB
MB
MB
SB
mB
YB
?B
?B
YB
YB
yB
_B
_B
eB
eB
kB
kB
kB
qB
qB
xB
xB
xB
xB
]B
~B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
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
*�B
*�B
*�B
*�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2�B
2�B
3B
2�B
4B
4B
3�B
4B
4B
4B
4B
4B
4B
5B
5B
5B
5B
5B
5B
5B
5B
6B
6B
6B
6B
6+B
7B
6�B
6�B
7B
88B
88B
8B
9$B
9$B
9>B
9>B
9>B
9$B
:*B
:*B
:B
:B
;0B
;0B
<6B
<B
<6B
<6B
<6B
=<B
=<B
=<B
=<B
=<B
=<B
=<B
=<B
="B
>BB
>BB
?HB
?HB
?HB
?HB
?HB
?HB
?HB
@OB
@iB
@OB
AUB
AUB
AUB
B[B
BuB
B[B
BuB
B[B
B[B
B[B
CaB
CaB
DMB
DgB
DMB
DgB
DgB
DgB
EmB
FtB
FtB
FtB
FtB
FtB
GzB
GzB
GzB
G�B
GzB
H�B
H�B
H�B
I�B
I�B
J�B
I�B
I�B
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
L~B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
^B
^B
^B
_B
_B
_!B
`B
`B
aB
`B
aB
aB
aB
aB
`�B
bB
bB
bB
bB
c B
c B
c:B
c B
c B
c B
c B
d@B
d&B
d&B
d&B
d@B
e,B
e,B
e,B
eFB
eB
e,B
e,B
e,B
f2B
f2B
e,B
e,B
eFB
e,B
f2B
f2B
g8B
g8B
g8B
g8B
g8B
g8B
g8B
g8B
g8B
h>B
h$B
h>B
h$B
iDB
h>B
h$B
hXB
i*B
iDB
i*B
iDB
iDB
iDB
jKB
jKB
j0B
jeB
jKB
jKB
jKB
jKB
jKB
jKB
jKB
kQB
kkB
kQB
lWB
lWB
lWB
lWB
lWB
m]B
m]B
mwB
mCB
m]B
ncB
ncB
n}B
ncB
ncB
ncB
ncB
ncB
ncB
n}B
oiB
oiB
poB
poB
pUB
qv111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.4(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803250042262018032500422620180325004226201804060311372018040603113720180406031137JA  ARFMdecpA19c                                                                20180319183527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180319093535  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180319093536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180319093536  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180319093537  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180319093537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180319093537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180319093537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180319093537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180319093537                      G�O�G�O�G�O�                JA  ARUP                                                                        20180319095528                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180319154014  CV  JULD            G�O�G�O�F¤l                JM  ARCAJMQC2.0                                                                 20180324154226  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180324154226  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181137  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                