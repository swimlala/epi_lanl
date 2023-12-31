CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-09T00:35:23Z creation;2017-11-09T00:35:27Z conversion to V3.1;2019-12-19T07:53:03Z update;     
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
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  s   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171109003523  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_178                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�3�فT 1   @�3��8�@4b���m�d��Ϫ͟1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@��@��A�
A?�
A_�
A�
A��A��A��A��RA��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC^
C_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D@�D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd��Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Djx�Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D��D�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�C�A�C�A�A�A�C�A�E�A�S�A�jAۅAۮA۶FAۡ�Aۏ\A�hsA�bA��mAڶFA�t�A�&�A���A�`BA�=qA�=qA�33A�-A�&�A��A���AظRA׃A���A�z�A��A��AоwAΙ�A��A�E�A��A��`A�v�A�bA�hsA�7LA���A�bA�hsA�Q�A�A��A��/A�|�A��TA��;A�33A�^5A�A� �A�%A��A�I�A���A���A�XA� �A��
A��A� �A�-A���A��jA��/A�t�A���A�=qA��`A��A�1A�r�A�x�A�p�A��A��9A�t�A��/A�$�A�C�A�VA�t�A�(�A�oA�r�A���A�dZA�r�A���A�E�A�A�l�A�&�A��7A��A�&�A�A� �A�A�A}%A{�-Az$�AxȴAv�+Au&�At��At^5Aq�Ao%An1'Amx�Akx�Ah5?AfE�Ae��Ad�\Ac�7Aa�A_�wA_oA^�A];dA[�FAY�AU�AT=qAQ�#AQ�APȴAP�AN�AL�\AK�AJ=qAI"�AHI�AG+AF~�AC`BAB�AA;dA?��A>$�A=A;�A:r�A9�hA9;dA85?A7��A6��A5�hA4�9A3?}A2{A1�A/�-A.�/A-�A-�-A,{A)�A(n�A'+A%�;A%G�A%K�A%t�A%�hA%t�A#ƨA"n�A!�^A  �A�AC�A�FA�RAXAA�^A33A�AA��A�;AQ�A�;A�PAdZA33A�A�HA�uAffAQ�AA+A�A��A-A
ĜA	�A	��A�AbNA��A��A+AA�HA�\AjAE�A�-AA �+@�
=@�r�@�33@���@�1@�^5@� �@��@�@�o@�V@�j@�A�@�\)@���@�@旍@��#@��@�R@�7@�Z@�"�@���@���@܃@�(�@ڗ�@�l�@��@ָR@֏\@���@�9X@�|�@�/@ϥ�@�^5@���@́@��`@���@�"�@���@�ƨ@Ƨ�@�n�@�{@�x�@ļj@��m@�"�@\@�J@���@�&�@�b@�+@��+@�@��#@��^@�p�@�9X@�ƨ@���@�\)@�
=@���@���@��@�9X@���@��y@���@���@�v�@��T@���@�hs@�%@��@�I�@�b@���@�C�@�ȴ@��+@�M�@�5?@�J@���@��7@�hs@�?}@�Ĝ@��m@���@���@�t�@�K�@�E�@�`B@��@��j@��D@�j@�A�@�  @�l�@�o@��H@���@�~�@�=q@���@��7@�x�@�V@�r�@�Ĝ@�%@��@�bN@��m@�n�@�@���@���@���@���@�`B@�G�@�&�@�Ĝ@���@��@��F@���@�t�@�K�@�"�@��\@��@���@��@�X@�O�@�V@���@�z�@�A�@��@���@��@��@�C�@��H@��R@���@�^5@�n�@��y@��@���@�?}@�z�@�j@�1@�bN@��h@�@�ff@�E�@��h@��@�`B@�?}@��@�I�@���@���@��@���@��
@���@��@�t�@�\)@�C�@�33@�"�@�
=@��!@�v�@�^5@�5?@��@��h@��@�Z@��;@���@��@���@�;d@��!@���@���@�ff@�5?@��#@��@�`B@��@��D@��m@���@��@�l�@�\)@�K�@�+@�"�@�
=@��y@��@���@��+@�^5@�5?@���@��T@��7@�%@���@���@��D@�z�@�Z@�1'@�b@��P@�@���@���@�M�@��@�@��-@���@��@�?}@��@���@��j@�j@��@�  @��m@��
@���@�t�@���@���@��\@�v�@�=q@�{@���@�?}@��@� �@��
@��F@�l�@�@��R@��+@�@�hs@�X@�G�@�7L@��@���@�j@��@��;@��
@��@�ƨ@�;d@�o@�C�@��+@�^5@��!@�o@�@�
=@��H@���@�M�@�-@���@��#@���@���@��@�7L@��@�Ĝ@��u@�A�@�w@�P@+@
=@~�@~�R@~v�@~{@}��@}p�@}?}@|��@{��@{��@{t�@{C�@z��@zn�@zJ@yG�@x��@xr�@w�@w
=@v�R@vV@v@u��@u��@u/@t��@tZ@t(�@s��@s��@sS�@r�@r��@r�!@r~�@rn�@q��@q�^@qhs@p��@p�`@p�@o\)@o�@nv�@m�-@m/@l��@l�@ko@jn�@j-@jJ@i��@i��@i��@i��@ix�@ihs@i�@h�u@hb@g\)@f�y@f��@f��@fv�@e��@e?}@e/@eV@d�@dj@d1@c�F@c��@c�@c33@b��@a�@a�#@a�#@a�#@aG�@`�9@`  @_K�@^�R@^��@^E�@]��@]p�@\�j@\9X@\�@[�m@[ƨ@[��@[t�@[@Z~�@ZJ@Y�#@YX@X�`@X��@X  @WK�@Vv�@VV@U�@U�-@U�h@U/@T�@TI�@Sƨ@St�@S"�@R�@R��@R��@R-@Q&�@P�@P1'@P �@Pb@Pb@O��@O\)@O+@N��@Nȴ@N��@NV@M�T@L�j@K��@K��@K�@K33@J�H@J�!@J��@J�\@J^5@J�@I��@Ix�@I&�@H�9@HbN@HQ�@HA�@H1'@H  @G�w@G|�@G+@G�@F��@F�@Fȴ@F��@F�+@F5?@F{@E�T@EO�@E/@EV@D��@DZ@C��@C�
@C�F@Ct�@C33@Co@B�@B��@B�\@B^5@B�@A�@A�^@A�7@Ahs@AX@A7L@A%@@��@@�@@A�@@1'@?�;@?��@?+@>�@>v�@>E�@=`B@<��@<�D@<(�@;�m@;ƨ@;��@;S�@:��@:-@:J@:J@:J@9��@9�7@9hs@9�@8��@8Ĝ@8�@81'@7�P@7K�@7;d@7+@6��@6�@6ȴ@6ȴ@6��@6V@6$�@6@5�-@5/@4��@4�j@4�@4z�@4I�@41@3�F@3S�@2�@2�!@2��@2�\@2~�@2-@1hs@1%@0Ĝ@0r�@0 �@0b@/�@/�w@.ȴ@.V@.$�@-�@-�T@-�@,��@,��@,9X@+�
@+��@+�@+33@*��@*=q@)��@)��@)��@)��@)x�@)&�@(��@(r�@(A�@( �@(  @'��@'��@'��@'\)@';d@&��@&�@&��@&V@&{@%@%p�@%`B@%/@%/@%/@%�@$�@$z�@$Z@$�@#��@#�m@#�F@#��@#dZ@#C�@#33@#o@"��@"��@"~�@"^5@"M�@"M�@"M�@"=q@"-@!��@!x�@!X@!G�@!&�@!�@ ��@ �`@ �`@ ��@ �u@  �@��@�P@K�@��@ȴ@��@V@5?@$�@@@O�@�@��@��@�j@�@�D@Z@Z@I�@1@ƨ@��@�@33@��@=q@�@J@�#@�^@��@hs@7L@&�@%@��@�`@��@��@��@r�@Q�@1'@ �@ �@ �@�@��@�w@�w@�@\)@+@�@
=@��@�y@�y@�y@�@��@v�@$�@{@{@{@{@@@�T@@�h@`B@/@�/@��@��@��@��@�D@�D@j@Z@��@ƨ@��@��@�@�@S�@"�@@@�@��@��@��@�!@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�C�A�C�A�A�A�C�A�E�A�S�A�jAۅAۮA۶FAۡ�Aۏ\A�hsA�bA��mAڶFA�t�A�&�A���A�`BA�=qA�=qA�33A�-A�&�A��A���AظRA׃A���A�z�A��A��AоwAΙ�A��A�E�A��A��`A�v�A�bA�hsA�7LA���A�bA�hsA�Q�A�A��A��/A�|�A��TA��;A�33A�^5A�A� �A�%A��A�I�A���A���A�XA� �A��
A��A� �A�-A���A��jA��/A�t�A���A�=qA��`A��A�1A�r�A�x�A�p�A��A��9A�t�A��/A�$�A�C�A�VA�t�A�(�A�oA�r�A���A�dZA�r�A���A�E�A�A�l�A�&�A��7A��A�&�A�A� �A�A�A}%A{�-Az$�AxȴAv�+Au&�At��At^5Aq�Ao%An1'Amx�Akx�Ah5?AfE�Ae��Ad�\Ac�7Aa�A_�wA_oA^�A];dA[�FAY�AU�AT=qAQ�#AQ�APȴAP�AN�AL�\AK�AJ=qAI"�AHI�AG+AF~�AC`BAB�AA;dA?��A>$�A=A;�A:r�A9�hA9;dA85?A7��A6��A5�hA4�9A3?}A2{A1�A/�-A.�/A-�A-�-A,{A)�A(n�A'+A%�;A%G�A%K�A%t�A%�hA%t�A#ƨA"n�A!�^A  �A�AC�A�FA�RAXAA�^A33A�AA��A�;AQ�A�;A�PAdZA33A�A�HA�uAffAQ�AA+A�A��A-A
ĜA	�A	��A�AbNA��A��A+AA�HA�\AjAE�A�-AA �+@�
=@�r�@�33@���@�1@�^5@� �@��@�@�o@�V@�j@�A�@�\)@���@�@旍@��#@��@�R@�7@�Z@�"�@���@���@܃@�(�@ڗ�@�l�@��@ָR@֏\@���@�9X@�|�@�/@ϥ�@�^5@���@́@��`@���@�"�@���@�ƨ@Ƨ�@�n�@�{@�x�@ļj@��m@�"�@\@�J@���@�&�@�b@�+@��+@�@��#@��^@�p�@�9X@�ƨ@���@�\)@�
=@���@���@��@�9X@���@��y@���@���@�v�@��T@���@�hs@�%@��@�I�@�b@���@�C�@�ȴ@��+@�M�@�5?@�J@���@��7@�hs@�?}@�Ĝ@��m@���@���@�t�@�K�@�E�@�`B@��@��j@��D@�j@�A�@�  @�l�@�o@��H@���@�~�@�=q@���@��7@�x�@�V@�r�@�Ĝ@�%@��@�bN@��m@�n�@�@���@���@���@���@�`B@�G�@�&�@�Ĝ@���@��@��F@���@�t�@�K�@�"�@��\@��@���@��@�X@�O�@�V@���@�z�@�A�@��@���@��@��@�C�@��H@��R@���@�^5@�n�@��y@��@���@�?}@�z�@�j@�1@�bN@��h@�@�ff@�E�@��h@��@�`B@�?}@��@�I�@���@���@��@���@��
@���@��@�t�@�\)@�C�@�33@�"�@�
=@��!@�v�@�^5@�5?@��@��h@��@�Z@��;@���@��@���@�;d@��!@���@���@�ff@�5?@��#@��@�`B@��@��D@��m@���@��@�l�@�\)@�K�@�+@�"�@�
=@��y@��@���@��+@�^5@�5?@���@��T@��7@�%@���@���@��D@�z�@�Z@�1'@�b@��P@�@���@���@�M�@��@�@��-@���@��@�?}@��@���@��j@�j@��@�  @��m@��
@���@�t�@���@���@��\@�v�@�=q@�{@���@�?}@��@� �@��
@��F@�l�@�@��R@��+@�@�hs@�X@�G�@�7L@��@���@�j@��@��;@��
@��@�ƨ@�;d@�o@�C�@��+@�^5@��!@�o@�@�
=@��H@���@�M�@�-@���@��#@���@���@��@�7L@��@�Ĝ@��u@�A�@�w@�P@+@
=@~�@~�R@~v�@~{@}��@}p�@}?}@|��@{��@{��@{t�@{C�@z��@zn�@zJ@yG�@x��@xr�@w�@w
=@v�R@vV@v@u��@u��@u/@t��@tZ@t(�@s��@s��@sS�@r�@r��@r�!@r~�@rn�@q��@q�^@qhs@p��@p�`@p�@o\)@o�@nv�@m�-@m/@l��@l�@ko@jn�@j-@jJ@i��@i��@i��@i��@ix�@ihs@i�@h�u@hb@g\)@f�y@f��@f��@fv�@e��@e?}@e/@eV@d�@dj@d1@c�F@c��@c�@c33@b��@a�@a�#@a�#@a�#@aG�@`�9@`  @_K�@^�R@^��@^E�@]��@]p�@\�j@\9X@\�@[�m@[ƨ@[��@[t�@[@Z~�@ZJ@Y�#@YX@X�`@X��@X  @WK�@Vv�@VV@U�@U�-@U�h@U/@T�@TI�@Sƨ@St�@S"�@R�@R��@R��@R-@Q&�@P�@P1'@P �@Pb@Pb@O��@O\)@O+@N��@Nȴ@N��@NV@M�T@L�j@K��@K��@K�@K33@J�H@J�!@J��@J�\@J^5@J�@I��@Ix�@I&�@H�9@HbN@HQ�@HA�@H1'@H  @G�w@G|�@G+@G�@F��@F�@Fȴ@F��@F�+@F5?@F{@E�T@EO�@E/@EV@D��@DZ@C��@C�
@C�F@Ct�@C33@Co@B�@B��@B�\@B^5@B�@A�@A�^@A�7@Ahs@AX@A7L@A%@@��@@�@@A�@@1'@?�;@?��@?+@>�@>v�@>E�@=`B@<��@<�D@<(�@;�m@;ƨ@;��@;S�@:��@:-@:J@:J@:J@9��@9�7@9hs@9�@8��@8Ĝ@8�@81'@7�P@7K�@7;d@7+@6��@6�@6ȴ@6ȴ@6��@6V@6$�@6@5�-@5/@4��@4�j@4�@4z�@4I�@41@3�F@3S�@2�@2�!@2��@2�\@2~�@2-@1hs@1%@0Ĝ@0r�@0 �@0b@/�@/�w@.ȴ@.V@.$�@-�@-�T@-�@,��@,��@,9X@+�
@+��@+�@+33@*��@*=q@)��@)��@)��@)��@)x�@)&�@(��@(r�@(A�@( �@(  @'��@'��@'��@'\)@';d@&��@&�@&��@&V@&{@%@%p�@%`B@%/@%/@%/@%�@$�@$z�@$Z@$�@#��@#�m@#�F@#��@#dZ@#C�@#33@#o@"��@"��@"~�@"^5@"M�@"M�@"M�@"=q@"-@!��@!x�@!X@!G�@!&�@!�@ ��@ �`@ �`@ ��@ �u@  �@��@�P@K�@��@ȴ@��@V@5?@$�@@@O�@�@��@��@�j@�@�D@Z@Z@I�@1@ƨ@��@�@33@��@=q@�@J@�#@�^@��@hs@7L@&�@%@��@�`@��@��@��@r�@Q�@1'@ �@ �@ �@�@��@�w@�w@�@\)@+@�@
=@��@�y@�y@�y@�@��@v�@$�@{@{@{@{@@@�T@@�h@`B@/@�/@��@��@��@��@�D@�D@j@Z@��@ƨ@��@��@�@�@S�@"�@@@�@��@��@��@�!@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�RB��B��B�B�B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B�B�fB�B��B�B�TB�`B�B=qB`BB[#BP�B]/BffBhsBhsBv�B�PB�=B�%B�bB�oB��B��B�bB}�B}�B�hB�+B�=B�+B�bB�B}�Bv�B�\B�oB�oB�VB�=B~�Bx�B~�Br�BjB_;BQ�B@�B"�B�BBB��B�B�B�HB�5B�B��B�jB��B�Bt�By�BjBO�B �B
�B
��B
�;B
��B
��B
�B
�=B
u�B
cTB
YB
G�B
5?B
.B
%�B
�B
VB
JB
	7B
B	�B	�)B	�)B	��B	�}B	��B	��B	��B	��B	�hB	{�B	{�B	y�B	s�B	dZB	S�B	C�B	#�B	49B	.B	,B	0!B	'�B	�B	VB	JB	1B	B	B��B�B�B��B��BȴB��B�}B�jB�9B�?B�?B�B�B��B��B��B�{B�\B�\B�1B�=B�+B�%By�Bl�Bw�Bs�Bt�Bw�B�B�1B�PB�1Bz�B~�B�Bx�Bo�BgmBk�Bm�BjBe`Br�Bp�Bq�BhsB`BB]/BYB_;BcTBe`BffBhsBhsBiyBjBiyBe`B]/BW
BT�BQ�BL�BM�BR�BN�BN�BL�BH�BG�BK�BK�BQ�BT�BR�BN�BJ�BK�BG�BD�BI�BJ�BG�BH�BH�BP�BN�BM�BO�B[#BYBT�BP�BQ�BO�BW
BS�B^5B]/B^5B_;BaHBcTBe`BcTB\)BXBn�Bp�Bo�Bl�BgmBm�BgmBo�Bv�B}�B~�B|�B{�B}�B|�B{�B�7B�hB�hB�\B�\B�oB�{B��B��B��B��B��B��B��B��B�B�B��B��B�!B�3B�-B�-B�-B�-B�3B�}B�}BBɺBɺB��BɺB��B��B��B��B�B�
B�B�)B�;B�`B�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B	%B		7B	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	#�B	-B	.B	-B	-B	49B	6FB	5?B	2-B	33B	33B	:^B	?}B	@�B	A�B	B�B	D�B	G�B	H�B	G�B	K�B	M�B	P�B	S�B	S�B	S�B	T�B	T�B	XB	]/B	`BB	bNB	dZB	dZB	ffB	jB	l�B	o�B	o�B	r�B	r�B	t�B	v�B	|�B	~�B	~�B	�B	�=B	�+B	�7B	�=B	�VB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�-B	�3B	�LB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�/B	�)B	�)B	�5B	�TB	�ZB	�fB	�fB	�fB	�fB	�mB	�fB	�fB	�fB	�mB	�fB	�mB	�mB	�mB	�sB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
  B
B	��B
B
B
B
B
B
B
B
%B
	7B
	7B
	7B
	7B
1B
+B
	7B
DB
JB
DB
JB
JB
VB
bB
PB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
#�B
!�B
$�B
#�B
#�B
%�B
%�B
%�B
$�B
&�B
'�B
(�B
(�B
(�B
)�B
(�B
(�B
)�B
(�B
'�B
(�B
'�B
)�B
)�B
+B
)�B
(�B
+B
,B
+B
+B
+B
+B
,B
-B
-B
,B
+B
+B
-B
0!B
/B
.B
.B
.B
.B
/B
1'B
0!B
0!B
1'B
1'B
2-B
49B
49B
49B
49B
49B
49B
33B
5?B
5?B
5?B
5?B
6FB
5?B
5?B
6FB
8RB
8RB
8RB
9XB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
:^B
9XB
7LB
9XB
:^B
;dB
<jB
;dB
;dB
:^B
;dB
;dB
<jB
<jB
;dB
:^B
9XB
:^B
<jB
=qB
=qB
=qB
>wB
?}B
>wB
>wB
>wB
@�B
?}B
A�B
A�B
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
D�B
D�B
D�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
M�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
T�B
S�B
S�B
VB
W
B
W
B
W
B
XB
XB
XB
VB
XB
ZB
ZB
ZB
YB
YB
ZB
ZB
[#B
\)B
\)B
\)B
[#B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
aHB
bNB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
cTB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
jB
k�B
k�B
jB
jB
k�B
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
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
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
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
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B� B�B�OB̘B�KB��B��B�8B�fB�2B�+B�MB�IB�WB�/B��B��B��B��B��B�	B�?B�B��B� B��B��B�XB��B�B@iBb4B^5BU�B`\Bi�BlWBl�By�B��B��B�fB��B��B�dB��B�oB�AB�;B�[B�#B�JB��B��B��B��ByrB��B��B�&B�BB�xB�;B{0B�BtnBk�B`�BS�BC�B&2BB�BmB�]B�AB��B��B߾B�	B�NB��B�RB�rBwB{dBl�BTFB'RB
�B
��B
�TB
��B
��B
��B
��B
w�B
fB
[WB
J=B
8�B
/�B
'�B
~B
�B
�B

	B
-B	�B	�VB	�IB	�{B	�[B	��B	��B	��B	�#B	�B	~�B	}qB	z�B	t�B	fLB	VmB	GB	($B	5�B	0�B	-B	0�B	)*B	�B	�B	�B	
=B	�B	9B�rB�ABܒBбB�MBʦBÖB�B��B�B�`B��B�cB��B�0B��B�)B�SB� B��B��B�^B��B�+B|PBo�By>ButBv+Bx�B�B�1B��B�B}<B��B�[Bz�Bq�Bi�BmwBoBlWBgBsBqvBr|Bi�Ba�B^�BZ�B_�Bc�Be�Bf�Bh�Bh�Bi�Bj�Bi�BfB^�BX�BVmBS[BN�BN�BS�BO�BO�BM�BJrBI�BMPBMBRTBUMBSuBO�BK�BL�BI7BF?BJ�BK�BIBJ	BJ=BQ�BPBOvBQ B[qBY�BU�BRBR�BQ4BW�BUgB_B^5B_!B`'Bb4Bc�Be�Bc�B]~BY�Bn�Bp�Bo�BmCBh�BncBiBp�Bw�B~wBcB}�B|�B~�B}�B}qB��B��B��B��B�B�B�B�B�	B��B�WB�kB�\B�`B�_B�=B�=B�eB��B�oB�hB�|B�|B��B��B�B��B�B��B��B��B��B�#B�B�4B�NB�TB�9B�YB�_B�xB߾B�B�B�B�B��B��B��B��B�B�-B�B�"B�"B�6B��B��B	YB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	$@B	-B	.IB	-]B	-]B	4B	6+B	5tB	2�B	3�B	49B	:�B	?�B	@�B	A�B	B�B	D�B	G�B	H�B	G�B	K�B	N<B	QNB	S�B	T,B	TFB	U2B	U�B	XyB	]~B	`vB	bhB	dtB	d�B	f�B	j�B	l�B	o�B	o�B	r�B	r�B	t�B	wB	}B	B	.B	�B	�#B	��B	��B	��B	��B	��B	��B	�jB	�,B	��B	��B	�CB	��B	�]B	�B	�GB	��B	��B	��B	�LB	�XB	�^B	�dB	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	�B	�+B	�B	��B	��B	��B	�.B	�4B	�B	�$B	�EB	�KB	�eB	�kB	�~B	�xB	ܒB	ޞB	�B	�tB	�B	�B	�B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�$B	�$B	�B	�B	�B	�"B	�B	�B	�6B	�B
 4B
 B
 4B
 4B
 OB
 4B
UB	��B
AB
3B
GB
gB
mB
SB
�B
YB
	RB
	RB
	RB
	RB
fB
�B
	lB
^B
dB
^B
dB
�B
VG�O�B
�B
}B
@B
yG�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
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
"�B
#B
$B
$�B
$B
"4B
%B
$&B
$&B
&B
&2B
&2B
%FB
'B
($B
(�B
(�B
)B
)�B
(�B
(�B
*B
)*B
($B
)DB
(>B
*0B
*B
+B
*B
)_B
+B
,"B
+B
+6B
+B
+6B
,"B
-)B
-CB
,=B
+6B
+6B
-)B
0!B
/5B
.IB
.IB
.cB
.cB
/5B
1AB
0UB
0UB
1[B
1vB
2aB
4TB
4TB
4TB
4TB
4TB
4nB
3hB
5ZB
5tB
5�B
5�B
6`B
5�B
5�B
6�B
8lB
8lB
8lB
9rB
8�B
8�B
8�B
9�B
9rB
9rB
:xB
;B
:xB
9�B
7�B
9�B
:xB
;dB
<jB
;B
;B
:�B
;B
;B
<�B
<�B
;�B
:�B
9�B
:�B
<�B
=�B
=�B
=�B
>�B
?}B
>�B
>�B
>�B
@�B
?�B
A�B
A�B
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
D�B
D�B
D�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
J�B
J	B
KB
MB
MB
M�B
M�B
M�B
M�B
MB
NB
O�B
P�B
O�B
PB
O�B
O�B
O�B
O�B
Q B
O�B
PB
P.B
QB
Q�B
R B
RB
RB
R�B
SB
R B
RB
RB
RB
Q�B
R B
SB
UB
TB
TB
TB
TB
T,B
T,B
TB
UB
VB
V9B
UB
T,B
TaB
VB
W$B
W$B
W$B
X+B
X+B
X+B
VSB
XEB
Z7B
Z7B
Z7B
YKB
YKB
ZkB
ZQB
[=B
\CB
\CB
\CB
[WB
\]B
]IB
^OB
^5B
^5B
^jB
]dB
]~B
^OB
_VB
_VB
_VB
_VB
`BB
_VB
_VB
`\B
_VB
`\B
`\B
`\B
`\B
`\B
abB
bNB
abB
bhB
bNB
bhB
abB
a|B
bhB
bhB
cTB
cnB
cnB
c�B
cnB
cnB
dZB
cnB
cnB
d�B
d�B
dtB
e`B
ezB
e`B
e`B
dtB
c�B
ezB
ezB
ffB
f�B
ffB
f�B
ffB
ffB
f�B
ezB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
iyB
hsB
h�B
h�B
i�B
j�B
jB
jB
jB
j�B
j�B
k�B
j�B
j�B
j�B
k�B
k�B
j�B
j�B
k�B
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
m�B
m�B
n�B
n�B
n}B
n�B
o�B
n�B
n�B
o�B
o�B
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
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
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
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711130033542017111300335420171113003354201806221321442018062213214420180622132144201804050724382018040507243820180405072438  JA  ARFMdecpA19c                                                                20171109093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171109003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171109003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171109003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171109003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171109003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171109003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171109003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171109003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171109003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20171109005521                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171109153803  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171110000000  CF  PSAL_ADJUSTED_QCD�� D�  G�O�                JM  ARCAJMQC2.0                                                                 20171112153354  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171112153354  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222438  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042144  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                