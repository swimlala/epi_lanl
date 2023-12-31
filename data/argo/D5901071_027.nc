CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:58Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135654  20190522121825  1727_5046_027                   2C  D   APEX                            2143                            040306                          846 @�8�I��1   @�8���_�@7hr� Ĝ�c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dy� D�&fD�l�D��3D��fD�&fD�ffD���D�� D��D�S3D���D��fD�0 D�Y�D�|�D��3D�#3D�ffD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C�3C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;�3C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg� Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.` D.�fD/ffD/�fD0ffD0�fD1` D1�fD2ffD2�fD3ffD3�fD4ffD4��D5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEl�DE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa� DbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk��DlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsY�Dy�fD��D�` D��fD��D��D�Y�D�� D�s3D� D�FfD���D�ٚD�#3D�L�D�p D��fD�fD�Y�D�|�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AжFAиRAк^Aк^AмjAоwAоwA���A���A���A�A�A�A�A�A�A���A�A�ĜA�AжFAжFAд9Aв-AУ�A�+A�ĜA��A�33A�jA�VA� �A�bNA���A�9XA�ĜA�?}A��wA�oA���A��A�dZA���A�n�A��A���A� �A��jA�?}A��mA���A�S�A��A�9XA�(�A�1A��A���A��hA�I�A�ĜA��\A�x�A�Q�A�;dA�1A���A�^5A�9XA��A��+A�n�A��A��jA���A�t�A��A���A�A���A�z�A�bA���A��A�C�A��A���A��PA�K�A��TA�r�A�{A���A��A�G�A�ȴA��A���A�9XA��A�z�A���A��
A�G�A���A�O�A��7A�I�A��A���A�C�A�%A�A�A��`A���A�I�A�S�A��-A�E�A� �A���A��mA�/A�(�A��HA���A�ZA��+A���A��A���A�A�VA��9A���A��A~�jA}�A}33A{�TA{AzAy�Axn�Au�#Au|�Au7LAsC�Aq�AnjAk��AgS�Acx�Aa�A^��AZ�AX��AW�TAV�DAT��AT9XASt�AQ�AP��AO��AN��AM��ALQ�AK+AI�AGAF�AF�uAE�ABr�A@ �A>�!A=�PA<��A;�7A9O�A8bA7ƨA6�A6�HA8~�A8��A7�A7�-A7%A5�A3�PA2�A1�A0�A0A.��A-�TA-�A+�-A(��A&�9A%��A%�hA%7LA$E�A#/A!x�AdZA/A��A-A�PA�RA��AVA��A�uA�-A��A^5AI�AbA�A�`A�hAp�A&�AȴAA�A�wA��An�A/A��AI�AbA��A7LA
=A��Az�A7LAbA
^5A	�TA	S�A��A��A��A1'A?}A�`AȴA�jA��A�A�\A��A7L@���@���@��`@�z�@�1'@��
@��@�dZ@��@���@�v�@��h@�?}@��D@�K�@�D@�|�@�33@�v�@�(�@��^@��@�u@띲@�\@��@�X@���@���@��@�@��D@�I�@�A�@߾w@�+@�@ܼj@�\)@ڸR@ٙ�@�1'@�ȴ@�@ԋD@�$�@�x�@�V@϶F@��y@��@�A�@�33@���@ʗ�@ʇ+@ʇ+@�=q@���@Ȭ@�l�@���@ź^@���@�dZ@¸R@��@��\@��#@��@��u@���@�C�@�o@��\@���@�%@��u@�9X@���@�|�@�^5@�x�@��m@�K�@��y@��@���@�9X@���@�1@���@��y@���@�X@�I�@���@���@���@��@��@�@��^@�?}@�V@��/@���@��;@�33@��y@�~�@�-@�%@��9@�A�@�1'@�t�@�
=@�n�@��@�?}@��@���@��@���@��@�Z@�1@���@��@�K�@���@�=q@�hs@�Z@�  @��m@���@��w@��@���@��P@�t�@�;d@�;d@�5?@�`B@��D@��w@��@���@���@��P@��@�t�@�C�@���@�n�@�5?@���@�p�@���@�bN@���@�+@��y@���@���@�v�@�-@���@��T@���@�@��^@�@���@��7@��h@�V@���@���@��D@�z�@�r�@�9X@�A�@��@�  @��;@�ƨ@��w@��F@���@���@���@��P@�dZ@�@�E�@�J@��#@��#@��-@�hs@�O�@�7L@�&�@���@��u@�Q�@�1'@���@���@���@�\)@�o@��y@���@�^5@�$�@�{@��@��@��@�{@��@~E�@r��@kt�@a�@[�m@V�y@O�@I7L@A��@:��@5�@/\)@(r�@"�@O�@�;@��@��@Ĝ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AжFAиRAк^Aк^AмjAоwAоwA���A���A���A�A�A�A�A�A�A���A�A�ĜA�AжFAжFAд9Aв-AУ�A�+A�ĜA��A�33A�jA�VA� �A�bNA���A�9XA�ĜA�?}A��wA�oA���A��A�dZA���A�n�A��A���A� �A��jA�?}A��mA���A�S�A��A�9XA�(�A�1A��A���A��hA�I�A�ĜA��\A�x�A�Q�A�;dA�1A���A�^5A�9XA��A��+A�n�A��A��jA���A�t�A��A���A�A���A�z�A�bA���A��A�C�A��A���A��PA�K�A��TA�r�A�{A���A��A�G�A�ȴA��A���A�9XA��A�z�A���A��
A�G�A���A�O�A��7A�I�A��A���A�C�A�%A�A�A��`A���A�I�A�S�A��-A�E�A� �A���A��mA�/A�(�A��HA���A�ZA��+A���A��A���A�A�VA��9A���A��A~�jA}�A}33A{�TA{AzAy�Axn�Au�#Au|�Au7LAsC�Aq�AnjAk��AgS�Acx�Aa�A^��AZ�AX��AW�TAV�DAT��AT9XASt�AQ�AP��AO��AN��AM��ALQ�AK+AI�AGAF�AF�uAE�ABr�A@ �A>�!A=�PA<��A;�7A9O�A8bA7ƨA6�A6�HA8~�A8��A7�A7�-A7%A5�A3�PA2�A1�A0�A0A.��A-�TA-�A+�-A(��A&�9A%��A%�hA%7LA$E�A#/A!x�AdZA/A��A-A�PA�RA��AVA��A�uA�-A��A^5AI�AbA�A�`A�hAp�A&�AȴAA�A�wA��An�A/A��AI�AbA��A7LA
=A��Az�A7LAbA
^5A	�TA	S�A��A��A��A1'A?}A�`AȴA�jA��A�A�\A��A7L@���@���@��`@�z�@�1'@��
@��@�dZ@��@���@�v�@��h@�?}@��D@�K�@�D@�|�@�33@�v�@�(�@��^@��@�u@띲@�\@��@�X@���@���@��@�@��D@�I�@�A�@߾w@�+@�@ܼj@�\)@ڸR@ٙ�@�1'@�ȴ@�@ԋD@�$�@�x�@�V@϶F@��y@��@�A�@�33@���@ʗ�@ʇ+@ʇ+@�=q@���@Ȭ@�l�@���@ź^@���@�dZ@¸R@��@��\@��#@��@��u@���@�C�@�o@��\@���@�%@��u@�9X@���@�|�@�^5@�x�@��m@�K�@��y@��@���@�9X@���@�1@���@��y@���@�X@�I�@���@���@���@��@��@�@��^@�?}@�V@��/@���@��;@�33@��y@�~�@�-@�%@��9@�A�@�1'@�t�@�
=@�n�@��@�?}@��@���@��@���@��@�Z@�1@���@��@�K�@���@�=q@�hs@�Z@�  @��m@���@��w@��@���@��P@�t�@�;d@�;d@�5?@�`B@��D@��w@��@���@���@��P@��@�t�@�C�@���@�n�@�5?@���@�p�@���@�bN@���@�+@��y@���@���@�v�@�-@���@��T@���@�@��^@�@���@��7@��h@�V@���@���@��D@�z�@�r�@�9X@�A�@��@�  @��;@�ƨ@��w@��F@���@���@���@��P@�dZ@�@�E�@�J@��#@��#@��-@�hs@�O�@�7L@�&�@���@��u@�Q�@�1'@���@���@���@�\)@�o@��y@���@�^5@�$�@�{@��@��@��@�{@��@~E�@r��@kt�@a�@[�m@V�y@O�@I7L@A��@:��@5�@/\)@(r�@"�@O�@�;@��@��@Ĝ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBy�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�Bx�Bx�Bu�Bn�B^5B]/B[#BaHBo�Bu�B��B��B��B��B��B��B��B�B�dBĜB��B��B�B�5B�`B�B��B��B��BBVBVB\BhB{B{B�B�B �B �B"�B"�B#�B&�B(�B(�B)�B)�B(�B)�B+B,B,B-B-B.B.B-B-B1'B1'B1'B2-B1'B2-B5?B49B/B)�B#�B �B�BoBJBBBB��B�HB��BȴB��B�LB�B��B��B��B�7Bk�BF�B$�B%B�BȴB�XB�B�BZBI�B=qB)�B{BB
��B
�B
��B
�RB
�B
��B
z�B
cTB
W
B
I�B
B�B
?}B
=qB
A�B
?}B
:^B
7LB
1'B
'�B
$�B
 �B
�B
PB	��B	�;B	�}B	��B	�B	n�B	VB	I�B	E�B	7LB	.B	+B	&�B	!�B	�B	�B	{B	\B	1B��B��B�B�mB�TB�B��BƨBÖBB�}B�LB�B��B��B��B�9B�TB�B�B�B�B�ZB�#B��B��B��B��BĜB�wB�LB�B��B��B��B�{B�oB�JB�%B|�Bx�Bw�Bv�Bt�Bs�Bs�Bs�Bt�Bt�Br�Bs�Bt�Bu�Bt�Bt�Br�Bo�Bm�Bn�Bo�Bp�Bm�BjBiyBiyBffBgmBgmBiyBk�Bn�Bp�Bo�Bm�Bn�Bm�Bq�Bs�Bt�Bt�Bs�Bs�Br�Bq�Br�Br�Bs�Br�Bt�Bv�Bx�Bo�BhsBhsBiyBjBk�Bk�Bk�Bk�Bl�Bl�Bm�Bo�Br�Bu�Bz�B}�B� B� B}�B}�B~�B�B�B�B� B}�B}�B�B�+B�%B�B~�B� B�B�B�B�B�B�%B�%B�+B�7B�7B�=B�=B�PB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�3B�3B�'B�3B�3B�3B�3B�3B�9B�3B�9B�?B�LB�RB�XB�XB�XB�dB�qB��B��BBÖBŢBǮBɺB��B��B��B�B�B�/B�;B�;B�HB�NB�HB�NB�NB�`B�fB�mB�sB�B�B��B��B��B��B	  B	B	B	1B	
=B	PB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	-B	33B	33B	33B	5?B	7LB	9XB	:^B	8RB	8RB	8RB	;dB	@�B	D�B	J�B	L�B	L�B	N�B	R�B	VB	XB	\)B	`BB	aHB	bNB	hsB	k�B	l�B	n�B	n�B	n�B	n�B	o�B	q�B	q�B	r�B	s�B	v�B	w�B	x�B	}�B	�B	�B	�+B	�+B	�+B	�+B	�=B	�DB	�\B	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�FB	�LB	�LB	�RB	�RB	�XB	��B	�sB	��B
DB
�B
�B
(�B
2-B
9XB
@�B
F�B
L�B
S�B
YB
[#B
^5B
dZB
jB
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�Bx�Bz�By�Bz�Bs�B`BBbNBgmBr�B|�B��B��B��B��B��B��B�B�?B�wBƨB��B��B�B�BB�mB�B��B��B��B%B\B\BbBoB�B�B�B�B!�B!�B#�B#�B%�B(�B)�B,B+B+B+B,B,B-B.B.B.B/B/B0!B0!B2-B33B33B33B33B49B8RB7LB1'B,B$�B"�B�B{BhBBBB��B�`B��B��BÖB�dB�9B��B��B��B�\Bs�BL�B+BhB�;B��B�dB�LB�DB^5BM�BB�B1'B�B%BB
�NB
ÖB
�jB
�B
��B
�B
gmB
[#B
M�B
D�B
A�B
A�B
D�B
B�B
<jB
:^B
8RB
(�B
%�B
%�B
�B
�B	��B	�B	ɺB	��B	�7B	x�B	[#B	L�B	I�B	<jB	0!B	.B	-B	$�B	!�B	�B	�B	{B	JB	B��B�B�yB�mB�`B�B��BǮBŢBŢB�}B�!B��B�B��B�B�TB�B�B�B�B�B�;B�
B��B��B��BƨB��B�dB�FB��B��B��B��B��B�bB�DB�By�Bx�By�Bv�Bv�Bv�Bv�Bu�Bu�Bu�Bv�Bv�Bv�Bu�Bu�Bv�Bs�Bn�Bo�Bq�Br�Bo�Bm�Bk�Bl�BhsBhsBhsBjBl�Bo�Bq�Bp�Bp�Bq�Br�Bs�Bu�Bu�Bu�Bt�Bu�Bu�Br�Bs�Bs�Bt�Bu�By�B|�B}�Bt�Bk�BjBjBk�Bl�Bl�Bl�Bl�Bl�Bm�Bo�Bp�Bt�Bw�B� B� B�B�B�B�B� B�B�B�B�B~�B~�B�1B�7B�1B�+B� B� B�B�%B�+B�+B�1B�1B�1B�=B�JB�DB�PB�VB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�9B�FB�9B�9B�?B�?B�9B�?B�9B�FB�FB�RB�XB�^B�^B�dB�qB��B��BBĜBŢBŢBȴBɺB��B��B�B�B�B�;B�HB�HB�NB�`B�HB�ZB�TB�fB�mB�sB�yB�B�B��B��B��B��B	B	B	B		7B	DB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	-B	33B	33B	49B	6FB	8RB	;dB	<jB	:^B	:^B	:^B	:^B	@�B	D�B	J�B	L�B	M�B	O�B	S�B	W
B	YB	]/B	aHB	cTB	dZB	iyB	l�B	m�B	n�B	n�B	o�B	o�B	o�B	q�B	q�B	r�B	s�B	v�B	w�B	x�B	~�B	�B	�%B	�+B	�+B	�+B	�1B	�=B	�JB	�\B	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�FB	�LB	�LB	�RB	�RB	�XB	��B	�sB	��B
JB
�B
�B
(�B
2-B
9XB
@�B
G�B
L�B
S�B
YB
\)B
_;B
dZB
k�B
p�B
u�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446432012010314464320120103144643  AO  ARGQ                                                                        20111130135654  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135654  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144643  IP                  G�O�G�O�G�O�                