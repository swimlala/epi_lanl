CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-07T15:37:41Z creation;2020-02-07T15:37:47Z conversion to V3.1;2022-11-21T05:27:35Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ip   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ML   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ̌   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200207153741  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_200                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"����1   @�#UUU�@;���s�dxC,�zx1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@�z�@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B(\)B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B�ǮB���B���B���B���B�.B���B���B���B���B�.B�.B�.B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS��CU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D�|{D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�K�A�O�A�Q�A�O�A�VA�VA�XA�ZA�ZA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�^5A�^5A�`BA�bNA�bNA�dZA�dZA�`BA�^5A�`BA�dZA�ffA�hsA�hsA�ffA�ffA�hsA�hsA�hsA�dZA�bNA�bNA�bNA�`BA�`BA�XA�9XA�JA���A�ƨA��wA�A��9A���A��\A�K�A��9A�  A���A�r�A��A�l�A�ffA�jA�A�-A�$�A��+A��A�ZA�A�A�(�A���A��PA�A���A���A�
=A�(�A��RA��7A�
=A�JA�`BA�?}A�bNA��9A��A��/A��A��A�ZA�{A���A�bNA��A�
=A��A�ƨA�p�A�ƨAl�A|�uA|9XA{`BAz(�Ay�
Ay�hAx�HAxI�Aw�wAvI�AsAq�
Ap��Ap^5Ao\)Am"�Al9XAkK�Ai�
Ah�`AgO�AeXAcC�Aa�7A^�A]�TA]p�A]�A\�!A\A�A[�AY&�AX �AWAW
=AV �AU&�AS��AQ�APZAO&�AM�AL�AL��AL-AL1AK�AJbNAJ9XAIC�AH��AHJAG��AF��AE��AD�jAD�AC��AB�RAA/A@$�A?33A>�yA>��A>�A=�A<��A:��A9�#A9hsA8�RA8�A7�TA7�A5�PA5;dA57LA5/A4�9A4~�A4ZA41'A3�A3\)A2I�A1��A1�-A0��A0ZA0-A/hsA. �A-dZA-O�A,�A+�A*ȴA*$�A)ƨA)"�A(��A'K�A&��A%�wA%&�A$ĜA$I�A"ĜA!��A!��A!x�A �`A��AXA��A1A��AdZA$�A��A��AȴA�hAffA�PA33A�9AZA\)A�DA^5AA�A33AȴA^5Ar�A��A
�yA	7LAv�A�A��A��Ar�A1AXA��AȴA�jAffA�AoA �A {@�+@�$�@�Z@�+@���@���@��@���@��@�v�@��h@�9X@�
=@��@��@��@�J@�9X@�v�@�G�@睲@�\@���@䛦@�"�@��H@�M�@�@�I�@�@�%@ۅ@��@�V@�t�@�K�@�
=@֏\@�E�@��@�r�@��y@щ7@ЋD@�  @�\)@Ώ\@�n�@�{@�7L@�A�@�t�@�^5@�X@�9X@�
=@ź^@��`@���@���@�V@��9@�Z@��P@�v�@���@��@��H@���@��u@��@�?}@�7L@�?}@�V@�1@���@�\)@��R@�M�@��@��7@�/@���@�Q�@�@��@��
@�C�@��R@��@�G�@�z�@���@���@�bN@�  @�33@���@�ƨ@�o@���@��9@�A�@�b@��
@�ƨ@�\)@�~�@��-@�`B@�G�@�/@��@���@��/@��9@�A�@���@�t�@�o@��y@��R@�$�@��-@��^@��7@��`@���@�z�@�j@��
@���@��P@�t�@��@���@�M�@��#@���@��@�hs@�?}@���@��@��@��m@���@�dZ@���@�=q@���@���@�hs@�?}@��j@� �@��@�|�@���@���@�V@���@��#@���@�7L@�I�@��
@���@���@�t�@�;d@�ȴ@��!@��!@��+@�$�@��#@��7@��/@��u@�r�@�1'@���@��w@�l�@���@��y@���@��!@�n�@��@��@��-@�x�@�X@�?}@��@���@���@�bN@�9X@� �@�  @�w@�@��@K�@~��@~ȴ@~5?@~$�@}�@}�-@}�h@}/@|��@|�@{��@z�\@y��@y�#@y�7@yG�@y�@xbN@w��@v�R@vff@v5?@u�@u�-@u�h@u�h@up�@tZ@sdZ@r�@q��@q7L@p�9@p  @o��@o�@o|�@oK�@o+@n�y@n�R@nff@n$�@m�@m�h@m/@l�j@l9X@k��@k�
@kt�@kS�@k@j�!@j~�@j�@i��@i��@i��@i��@i�7@hĜ@gK�@g+@fȴ@fff@f$�@e��@ep�@d��@d�@c�m@c�@ct�@cdZ@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cdZ@c"�@c@b��@b��@bn�@b�@`��@`Ĝ@`�9@`��@`�@`b@_l�@^��@^E�@^@^@]��@]O�@]/@]V@\��@\��@\��@\j@[��@[dZ@[C�@["�@Z��@Z~�@Z�@Yhs@YG�@X�`@X �@W��@W|�@V�@V@U@U�@UV@T�j@Tz�@S��@S��@S�@S��@S��@S�m@S�
@SS�@S"�@R�!@R^5@R=q@Q&�@PĜ@P��@PA�@O�@O�w@O�@O�@O
=@N��@N�+@Nff@M�@M@L��@L�@L��@L�@L�D@Lz�@Lz�@L9X@Kƨ@K�@J��@I��@HĜ@H �@G�P@G+@F�y@Fv�@F$�@F{@E��@D�j@D9X@DI�@Dj@C��@C�@CC�@B�H@B~�@B-@BJ@A��@A�#@A�^@Ax�@Ahs@AX@AX@A��@BM�@BM�@BJ@A��@Ahs@@��@@�`@@�`@@��@@�`@@�`@@��@@�9@@��@@Ĝ@@��@@bN@@Q�@@1'@@  @?�;@?\)@>�+@>$�@=�@=��@=@=@=�-@=��@=��@=�h@=p�@=V@=V@<j@<�@;ƨ@;��@;t�@;33@:�@:�H@:��@:n�@:�@9�#@9��@9G�@9%@8�9@8r�@8r�@8 �@7��@7��@7K�@6$�@5�h@5`B@5O�@5O�@5O�@5V@4��@4�@4j@4I�@3��@3�@333@2��@2�@2�@2��@2��@2��@2��@2~�@2J@1�^@1��@1&�@0�9@0�9@0�@0b@/�@/�;@/�w@/��@/l�@/
=@.ȴ@.�R@.v�@.@-��@-��@-�h@-p�@-O�@-/@,��@,�/@,�@,j@,I�@,(�@,1@+��@+ƨ@+��@+C�@+"�@+o@*�@*��@*�\@*n�@*M�@*=q@*-@)�@)��@(�9@(Q�@(1'@(1'@( �@'��@'|�@'K�@'�@&�y@&�+@&E�@&$�@&{@%�@%��@%@%p�@%?}@$�@$��@$�D@$Z@$�@#�F@#�@#dZ@#C�@#"�@"�H@"�!@"n�@!��@!�@!�#@!�^@!��@!x�@!G�@ ��@ �9@ �@ Q�@ 1'@ b@�@�;@�@|�@K�@+@�@�y@ȴ@v�@{@@��@p�@?}@/@�@V@�/@��@j@1@�
@ƨ@�F@��@��@�@C�@"�@�@�!@~�@J@��@hs@7L@&�@%@�9@1'@�P@l�@��@��@�+@V@E�@E�@5?@$�@��@�h@O�@?}@/@��@�/@�@��@��@z�@(�@1@�
@�F@��@�@o@��@~�@~�@~�@^5@��@��@��@��@hs@%@�`@��@Ĝ@�@Q�@1'@b@  @�w@|�@K�@�@��@�@��@��@�+@$�@@�T@��@`B@?}@?}@/@/@��@�j@�D@�D@�D@Z@(�@1@�
@t�@C�@"�@
��@
�!@
�\@
M�@
-@
�@	��@	�@	�@	��@	��@	��@	��@	��@	hs@��@r�@A�@  @�;@��@��@��@�w@�@�P@|�@l�@;d@+@
=@�y@�R@�R@�R@��@v�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�K�A�O�A�Q�A�O�A�VA�VA�XA�ZA�ZA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�^5A�^5A�`BA�bNA�bNA�dZA�dZA�`BA�^5A�`BA�dZA�ffA�hsA�hsA�ffA�ffA�hsA�hsA�hsA�dZA�bNA�bNA�bNA�`BA�`BA�XA�9XA�JA���A�ƨA��wA�A��9A���A��\A�K�A��9A�  A���A�r�A��A�l�A�ffA�jA�A�-A�$�A��+A��A�ZA�A�A�(�A���A��PA�A���A���A�
=A�(�A��RA��7A�
=A�JA�`BA�?}A�bNA��9A��A��/A��A��A�ZA�{A���A�bNA��A�
=A��A�ƨA�p�A�ƨAl�A|�uA|9XA{`BAz(�Ay�
Ay�hAx�HAxI�Aw�wAvI�AsAq�
Ap��Ap^5Ao\)Am"�Al9XAkK�Ai�
Ah�`AgO�AeXAcC�Aa�7A^�A]�TA]p�A]�A\�!A\A�A[�AY&�AX �AWAW
=AV �AU&�AS��AQ�APZAO&�AM�AL�AL��AL-AL1AK�AJbNAJ9XAIC�AH��AHJAG��AF��AE��AD�jAD�AC��AB�RAA/A@$�A?33A>�yA>��A>�A=�A<��A:��A9�#A9hsA8�RA8�A7�TA7�A5�PA5;dA57LA5/A4�9A4~�A4ZA41'A3�A3\)A2I�A1��A1�-A0��A0ZA0-A/hsA. �A-dZA-O�A,�A+�A*ȴA*$�A)ƨA)"�A(��A'K�A&��A%�wA%&�A$ĜA$I�A"ĜA!��A!��A!x�A �`A��AXA��A1A��AdZA$�A��A��AȴA�hAffA�PA33A�9AZA\)A�DA^5AA�A33AȴA^5Ar�A��A
�yA	7LAv�A�A��A��Ar�A1AXA��AȴA�jAffA�AoA �A {@�+@�$�@�Z@�+@���@���@��@���@��@�v�@��h@�9X@�
=@��@��@��@�J@�9X@�v�@�G�@睲@�\@���@䛦@�"�@��H@�M�@�@�I�@�@�%@ۅ@��@�V@�t�@�K�@�
=@֏\@�E�@��@�r�@��y@щ7@ЋD@�  @�\)@Ώ\@�n�@�{@�7L@�A�@�t�@�^5@�X@�9X@�
=@ź^@��`@���@���@�V@��9@�Z@��P@�v�@���@��@��H@���@��u@��@�?}@�7L@�?}@�V@�1@���@�\)@��R@�M�@��@��7@�/@���@�Q�@�@��@��
@�C�@��R@��@�G�@�z�@���@���@�bN@�  @�33@���@�ƨ@�o@���@��9@�A�@�b@��
@�ƨ@�\)@�~�@��-@�`B@�G�@�/@��@���@��/@��9@�A�@���@�t�@�o@��y@��R@�$�@��-@��^@��7@��`@���@�z�@�j@��
@���@��P@�t�@��@���@�M�@��#@���@��@�hs@�?}@���@��@��@��m@���@�dZ@���@�=q@���@���@�hs@�?}@��j@� �@��@�|�@���@���@�V@���@��#@���@�7L@�I�@��
@���@���@�t�@�;d@�ȴ@��!@��!@��+@�$�@��#@��7@��/@��u@�r�@�1'@���@��w@�l�@���@��y@���@��!@�n�@��@��@��-@�x�@�X@�?}@��@���@���@�bN@�9X@� �@�  @�w@�@��@K�@~��@~ȴ@~5?@~$�@}�@}�-@}�h@}/@|��@|�@{��@z�\@y��@y�#@y�7@yG�@y�@xbN@w��@v�R@vff@v5?@u�@u�-@u�h@u�h@up�@tZ@sdZ@r�@q��@q7L@p�9@p  @o��@o�@o|�@oK�@o+@n�y@n�R@nff@n$�@m�@m�h@m/@l�j@l9X@k��@k�
@kt�@kS�@k@j�!@j~�@j�@i��@i��@i��@i��@i�7@hĜ@gK�@g+@fȴ@fff@f$�@e��@ep�@d��@d�@c�m@c�@ct�@cdZ@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cS�@cdZ@c"�@c@b��@b��@bn�@b�@`��@`Ĝ@`�9@`��@`�@`b@_l�@^��@^E�@^@^@]��@]O�@]/@]V@\��@\��@\��@\j@[��@[dZ@[C�@["�@Z��@Z~�@Z�@Yhs@YG�@X�`@X �@W��@W|�@V�@V@U@U�@UV@T�j@Tz�@S��@S��@S�@S��@S��@S�m@S�
@SS�@S"�@R�!@R^5@R=q@Q&�@PĜ@P��@PA�@O�@O�w@O�@O�@O
=@N��@N�+@Nff@M�@M@L��@L�@L��@L�@L�D@Lz�@Lz�@L9X@Kƨ@K�@J��@I��@HĜ@H �@G�P@G+@F�y@Fv�@F$�@F{@E��@D�j@D9X@DI�@Dj@C��@C�@CC�@B�H@B~�@B-@BJ@A��@A�#@A�^@Ax�@Ahs@AX@AX@A��@BM�@BM�@BJ@A��@Ahs@@��@@�`@@�`@@��@@�`@@�`@@��@@�9@@��@@Ĝ@@��@@bN@@Q�@@1'@@  @?�;@?\)@>�+@>$�@=�@=��@=@=@=�-@=��@=��@=�h@=p�@=V@=V@<j@<�@;ƨ@;��@;t�@;33@:�@:�H@:��@:n�@:�@9�#@9��@9G�@9%@8�9@8r�@8r�@8 �@7��@7��@7K�@6$�@5�h@5`B@5O�@5O�@5O�@5V@4��@4�@4j@4I�@3��@3�@333@2��@2�@2�@2��@2��@2��@2��@2~�@2J@1�^@1��@1&�@0�9@0�9@0�@0b@/�@/�;@/�w@/��@/l�@/
=@.ȴ@.�R@.v�@.@-��@-��@-�h@-p�@-O�@-/@,��@,�/@,�@,j@,I�@,(�@,1@+��@+ƨ@+��@+C�@+"�@+o@*�@*��@*�\@*n�@*M�@*=q@*-@)�@)��@(�9@(Q�@(1'@(1'@( �@'��@'|�@'K�@'�@&�y@&�+@&E�@&$�@&{@%�@%��@%@%p�@%?}@$�@$��@$�D@$Z@$�@#�F@#�@#dZ@#C�@#"�@"�H@"�!@"n�@!��@!�@!�#@!�^@!��@!x�@!G�@ ��@ �9@ �@ Q�@ 1'@ b@�@�;@�@|�@K�@+@�@�y@ȴ@v�@{@@��@p�@?}@/@�@V@�/@��@j@1@�
@ƨ@�F@��@��@�@C�@"�@�@�!@~�@J@��@hs@7L@&�@%@�9@1'@�P@l�@��@��@�+@V@E�@E�@5?@$�@��@�h@O�@?}@/@��@�/@�@��@��@z�@(�@1@�
@�F@��@�@o@��@~�@~�@~�@^5@��@��@��@��@hs@%@�`@��@Ĝ@�@Q�@1'@b@  @�w@|�@K�@�@��@�@��@��@�+@$�@@�T@��@`B@?}@?}@/@/@��@�j@�D@�D@�D@Z@(�@1@�
@t�@C�@"�@
��@
�!@
�\@
M�@
-@
�@	��@	�@	�@	��@	��@	��@	��@	��@	hs@��@r�@A�@  @�;@��@��@��@�w@�@�P@|�@l�@;d@+@
=@�y@�R@�R@�R@��@v�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�^B�dB�dB�jB�jB�jB�jB�jB�dB�dB�dB�qB��B��B�PB~�BffBJ�BC�B<jB1'B(�B�B�BJB��B�mB�
B��B�dB��B�Bv�Br�Bp�BiyB\)BP�B@�B'�B�B�B\B
��B
�B
�sB
�TB
�)B
�B
��B
��B
ɺB
�wB
�LB
��B
�%B
t�B
p�B
k�B
dZB
aHB
_;B
ZB
VB
P�B
F�B
6FB
)�B
#�B
�B
�B
\B

=B
B	��B	��B	�B	�HB	�B	��B	�}B	�^B	�LB	�?B	�3B	�!B	��B	��B	��B	��B	�{B	�\B	�7B	�B	v�B	m�B	gmB	bNB	]/B	]/B	[#B	ZB	VB	W
B	VB	P�B	N�B	K�B	I�B	E�B	A�B	=qB	9XB	6FB	1'B	)�B	#�B	�B	�B	�B	�B	�B	uB	DB	%B	B	B��B��B��B�B�B�B�B�B�B�B�B�yB�fB�NB�BB�5B�B�B��B��B�}B�^BBB�qB�XB�?B�3B�!B��B��B��B��B��B��B�oB�VB�=B�7B�+B�B� B{�Bz�By�Bx�Bv�Bt�Bp�Bl�BjBhsBe`BcTBbNBaHB_;B^5B\)B[#BZBYBXBVBS�BP�BN�BK�BI�BF�BD�BB�B@�B>wB>wB=qB<jB<jB;dB:^B9XB8RB7LB8RB7LB6FB5?B49B49B33B33B2-B2-B1'B0!B0!B0!B1'B0!B/B.B,B)�B,B)�B(�B&�B%�B$�B%�B%�B&�B)�B+B+B+B)�B+B,B,B,B,B,B+B+B,B-B.B.B.B/B/B.B/B0!B0!B1'B2-B33B33B49B49B7LB8RB9XB9XB9XB:^B<jB>wB>wBB�BD�BE�BF�BL�BL�BK�BK�BM�BM�BM�BO�BO�BP�BP�BP�BP�BP�BT�BZB]/B_;BcTBffBiyBjBn�Bn�Bo�Bs�Bt�Bt�Bu�Bu�Bx�B|�B}�B~�B~�B~�B� B�B�+B�7B�7B�=B�=B�DB�DB�JB�VB�\B�hB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�LB�RB�RB�XB�jB��B��BBĜBŢBǮBɺB��B��B��B��B��B�B�)B�5B�TB�ZB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	+B	1B	JB	\B	\B	oB	{B	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	+B	+B	,B	0!B	1'B	33B	5?B	6FB	8RB	8RB	9XB	:^B	<jB	=qB	>wB	?}B	?}B	?}B	@�B	B�B	C�B	E�B	H�B	K�B	P�B	R�B	S�B	VB	W
B	W
B	ZB	^5B	aHB	cTB	dZB	e`B	e`B	ffB	ffB	ffB	jB	m�B	o�B	s�B	v�B	x�B	{�B	|�B	}�B	}�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�PB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�3B	�9B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�LB	�LB	�RB	�dB	�dB	�jB	�jB	�dB	�jB	�}B	��B	��B	B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�5B	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
PB
VB
VB
\B
\B
bB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
)�B
)�B
+B
+B
,B
-B
-B
.B
/B
/B
0!B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
2-B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
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
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
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
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
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
F�B
G�B
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
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
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
Q�B
Q�B
R�B
R�B
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
XB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
\)B
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
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
p�B
q�B
q�B
q�B
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
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�XB�rB��B��B��B��B��B�jB��B��B��B��B��B�B�B�B�1B�bB�aBj0BL�BE9B>�B2�B*�B!|BWBvBoB�B�B�NB��B��B�Bw�Bs�Br-Bk�B^5BS�BDB)�B�B�B B
��B
�B
�DB
�ZB
��B
ٴB
՛B
�[B
˒B
�iB
�JB
�,B
��B
u�B
q�B
l�B
d�B
a�B
`B
[	B
W$B
SB
IlB
8RB
+B
$�B
!bB
�B
�B
�B
�B	�wB	��B	�B	��B	�_B	�pB	��B	��B	��B	��B	�B	��B	�"B	��B	�WB	��B	��B	��B	�B	�gB	x�B	oB	h�B	cnB	]�B	]�B	[�B	[=B	V�B	W�B	W$B	Q�B	O�B	L�B	KB	F�B	B�B	>]B	:DB	7�B	2�B	+QB	$�B	 BB	;B	~B	kB	�B	�B	dB	�B	B	�B�wB��B��B�-B�B��B�5B��B��B��B�B�eB�B��B��B�!B��B֡B�9B�6B�iB��B�aB��B��B�*B��B�B�'B��B��B��B�kB�EB��B�,B�vB��B��B��B��B��B|�B{�Bz�By�Bx�Bv�Br-Bm�Bl"Bi�Bf�Bc�Bc Bb4B`�B_!B\�B[�BZ�BY�BX�BW$BVSBR:BP.BM�BJ�BG�BFtBD�BAUB?.B?cB=�B<�B<�B<B;0B:^B8�B8lB9$B88B7�B6B4�B4�B3�B3�B2�B33B2B1'B1B1'B2B1[B/�B/iB-CB+B-CB*�B)�B'�B&�B%FB&�B'B'�B+kB+�B,B+�B+QB+�B,=B,WB,qB,qB,�B,"B,"B,�B-�B.�B.�B.�B/iB/�B.�B/�B0�B0�B1�B3B4B49B5%B5�B8B8�B9�B9�B:B;0B="B?HB?�BC{BE�BF�BG�BL�BL�BLJBL~BN<BN"BNVBPHBPHBQNBQNBQhBQ�BRoBVBZ�B]�B_�Bc�BgBjKBk�Bo�Bo�BpBt�Bv`Bu�BvzBv�By�B}<B~(B.B.B}B��B��B�zB�RB�lB�XB�rB�xB��B��B��B��B��B��B��B��B��B��B��B�5B�B�B�B�@B�B�*B�0B�eB�wB��B��B�zB��B��B��B��B��B��B��B��B�B�B�1B��B�B�(B�B�TB�gB�eBܒB޸B�B�B�B�B��B��B�UB�B�$B��B�B�B�6B	 4B	 B	;B	[B	mB	zB	�B	~B	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	&B	($B	)*B	+B	+6B	,WB	0UB	1[B	3�B	5ZB	6`B	8lB	8lB	9rB	:�B	<�B	=�B	>�B	?�B	?�B	?�B	@�B	B�B	C�B	E�B	IB	L0B	QB	S&B	T,B	VB	W?B	WYB	Z�B	^�B	abB	cnB	dtB	ezB	ezB	f�B	f�B	f�B	j�B	m�B	p!B	s�B	v�B	y	B	|B	}B	~B	~B	�B	�B	� B	�AB	�3B	�9B	�?B	�fB	�rB	�xB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�;B	��B	�B	�B	�
B	�0B	�6B	�CB	�AB	�vB	�hB	�TB	�TB	�ZB	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�fB	�LB	�lB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�(B	� B	�B	�B	�B	�2B	�?B	�KB	�CB	�]B	ބB	�|B	�|B	�B	�B	�zB	�zB	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�RB	��B	��B	�B	�B	�B	�B	�B	�.B
 4B
 B
 B
AB
GB
GB
'B
3B
B
?B
+B
EB
KB
fB
fB
	�B

�B

�B
�B
jB
�B
pB
�B
vB
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
sB
B
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
!B
!�B
!�B
#B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$B
$�B
&B
'B
'B
'B
'B
'B
'B
'B
&�B
'B
'B
)B
*0B
*B
+B
+B
,"B
-B
-)B
.IB
/5B
/OB
0oB
/iB
/5B
/B
/B
/B
/5B
/5B
/5B
/5B
/5B
0UB
1AB
1AB
2GB
49B
49B
5ZB
5%B
5ZB
5ZB
6`B
6zB
7�B
8lB
8�B
8lB
9>B
9rB
9�B
:xB
:xB
:^B
:xB
:xB
:xB
;B
;dB
<�B
<�B
=�B
=qB
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?}B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
BuB
B�B
B�B
B�B
B�B
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
F�B
G�B
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
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
PB
O�B
O�B
O�B
O�B
O�B
QB
QB
Q B
Q B
RB
RB
R�B
SB
TB
TB
TB
TB
UB
UB
UB
UB
UB
UB
U2B
VB
VB
VB
VB
W
B
W$B
W?B
W$B
X+B
X+B
XEB
XEB
Y1B
Y1B
Y1B
YKB
YKB
ZQB
ZQB
[WB
[WB
\]B
\CB
\CB
]/B
]IB
]/B
]IB
]dB
]IB
^jB
^5B
^OB
^OB
_VB
_pB
aHB
aHB
a|B
abB
abB
aHB
bhB
bhB
bhB
bhB
b�B
bhB
bNB
bhB
bhB
bhB
cnB
cnB
cnB
c�B
cTB
dtB
dZB
d�B
dtB
e`B
ezB
ezB
ezB
ezB
f�B
f�B
f�B
g�B
g�B
g�B
gmB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
jB
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
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
p�B
q�B
q�B
q�B
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
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002180033272020021800332720200218003327202211182141582022111821415820221118214158202002200025022020022000250220200220002502  JA  ARFMdecpA19c                                                                20200208003735  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200207153741  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200207153744  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200207153744  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200207153745  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200207153745  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200207153745  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200207153745  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200207153745  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200207153745  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200207153746  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200207153747                      G�O�G�O�G�O�                JA  ARUP                                                                        20200207155431                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200207153247  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200207153223  CV  JULD            G�O�G�O�F�	                JM  ARCAJMQC2.0                                                                 20200217153327  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200217153327  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200219152502  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124158  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                