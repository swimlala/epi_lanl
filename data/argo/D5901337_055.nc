CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:39Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               7A   AO  20111205113248  20190522121836  1901_5055_055                   2C  D   APEX                            2140                            040306                          846 @Բf5��1   @Բ@yP@-$Z�1�cqV�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���@���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH�fDI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy� D��D�0 D�|�D��3D�3D�0 D�i�D��3D�fD�)�D�s3D��fD�� D�33D�ffD�fD��fD�#3D�i�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  @�  A  A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBV��B^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�3C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�3Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C�ٚC���C���C���C���C�� C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"��D#ffD#�fD$ffD$�fD%ffD%�fD&ffD&��D'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,� D-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7��D8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGl�DG��DHl�DH�fDIffDI�fDJffDJ�fDKffDK� DLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp� DqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyffD�  D�#3D�p D��fD��fD�#3D�\�D��fD���D��D�ffDǹ�D��3D�&fD�Y�Dਗ਼D�ٚD�fD�\�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��mA��A���A���A�  A�A�A�A�%A�%A�%A�1A�%A�A�
=A�1A�JA�VA�VA�VA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�{A�JA�A�  A�  A���A��/A�AĮA��PA�t�A���A�"�A��A��A��`A���A��A���A��A�&�A��A� �A��A���A�{A�K�A�bNA��A�"�A��A�VA�ZA�=qA�-A�p�A�33A���A�I�A�/A��A�x�A�A�|�A��jA���A��
A��+A�VA�-A���A��HA���A��A���A�  A�v�A��DA��A���A}VAxZAs�
Ajv�Ac��AbA_��A[�AUhsAS%AQ��AN��ALQ�AG�AE�AE�AD�\ADJAB5?AA&�A>ZA<�A:��A9�A6�uA1;dA/A/�hA/�-A/��A/p�A/"�A.�\A.n�A.  A-��A-?}A,�A,��A,�`A,�A,^5A+�A+l�A*�9A*bA*A)"�A(5?A'�A'\)A&jA%�wA$r�A#�FA#p�A"�A!A!7LA ��A�
AC�A��Ax�AA��A��A$�A�-A�PAl�A;dAoA�HAȴA�!A�DA5?A��A�AbA�A�\A��A�DA�PA7LAdZAS�A��A��AA�A��A��A��An�A  A��AK�A
=AĜA�\Az�AQ�A=qA�A�A��AI�AbA1A�TAO�A
�\A
A	�
A	�-A	S�A��A��AQ�A�FAS�A�A��AO�A��A�HA�uA�A/A�`A��AI�A=qA�A�7A ��@��@�=q@�M�@��@��-@��/@�ȴ@�V@��-@�Ĝ@���@��\@�5?@�J@�V@��;@���@�=q@�@��/@�I�@@�V@��^@�O�@���@��@띲@�@���@���@�&�@�@�r�@�Z@�(�@��;@�ƨ@�+@�-@��@�Q�@���@�S�@�"�@���@�E�@ᙚ@��`@�  @�S�@���@�j@�ƨ@ۍP@�@�^5@ى7@��@��@ם�@�S�@�@�/@Ԭ@�1@�|�@ҧ�@��@љ�@�?}@�V@���@�bN@Ͼw@�S�@��@Η�@Ͳ-@�V@�r�@�(�@�b@�\)@�ȴ@�ff@�$�@���@Ɂ@�j@Ǿw@��@�ȴ@Ə\@�=q@��@ź^@ŉ7@�G�@��@��/@ċD@�9X@�b@��m@Ý�@ÍP@�33@¸R@�=q@�@���@��9@���@�;d@��@���@�@��7@�G�@���@�Z@�b@�1@��
@��w@��F@��@�^5@�7L@�z�@�t�@�33@��@���@�ff@�x�@�?}@��@� �@�ƨ@�C�@��@�@��+@��@�7L@�Ĝ@�Z@�  @���@�l�@�
=@��@��@�V@��#@��@�%@���@��@�Q�@��@��@��@��\@�V@�-@�$�@�E�@�M�@�$�@�@�hs@�`B@�&�@�/@��D@�1'@���@�ȴ@���@�ff@�-@��@�J@��@��#@���@�X@�&�@���@���@��@�Z@�(�@���@�A�@�Q�@�9X@��;@�l�@�ȴ@��!@���@��\@�{@���@�`B@�%@��D@�9X@�1@��;@��
@�t�@��y@��R@�v�@��@��@�@�`B@��@�Ĝ@�z�@��@���@�S�@��@��y@���@�M�@�-@��-@��@��@��j@��u@�Z@�b@�ƨ@���@�l�@���@��\@��@��@��#@��h@�7L@�Ĝ@�I�@�b@��@��@�l�@�
=@���@��\@�V@�=q@��9@�~�@��j@��@u@l�@b~�@X �@N�+@HA�@?�;@7|�@0r�@)�7@#"�@O�@�\@?}@G�@(�@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��mA��A���A���A�  A�A�A�A�%A�%A�%A�1A�%A�A�
=A�1A�JA�VA�VA�VA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�{A�JA�A�  A�  A���A��/A�AĮA��PA�t�A���A�"�A��A��A��`A���A��A���A��A�&�A��A� �A��A���A�{A�K�A�bNA��A�"�A��A�VA�ZA�=qA�-A�p�A�33A���A�I�A�/A��A�x�A�A�|�A��jA���A��
A��+A�VA�-A���A��HA���A��A���A�  A�v�A��DA��A���A}VAxZAs�
Ajv�Ac��AbA_��A[�AUhsAS%AQ��AN��ALQ�AG�AE�AE�AD�\ADJAB5?AA&�A>ZA<�A:��A9�A6�uA1;dA/A/�hA/�-A/��A/p�A/"�A.�\A.n�A.  A-��A-?}A,�A,��A,�`A,�A,^5A+�A+l�A*�9A*bA*A)"�A(5?A'�A'\)A&jA%�wA$r�A#�FA#p�A"�A!A!7LA ��A�
AC�A��Ax�AA��A��A$�A�-A�PAl�A;dAoA�HAȴA�!A�DA5?A��A�AbA�A�\A��A�DA�PA7LAdZAS�A��A��AA�A��A��A��An�A  A��AK�A
=AĜA�\Az�AQ�A=qA�A�A��AI�AbA1A�TAO�A
�\A
A	�
A	�-A	S�A��A��AQ�A�FAS�A�A��AO�A��A�HA�uA�A/A�`A��AI�A=qA�A�7A ��@��@�=q@�M�@��@��-@��/@�ȴ@�V@��-@�Ĝ@���@��\@�5?@�J@�V@��;@���@�=q@�@��/@�I�@@�V@��^@�O�@���@��@띲@�@���@���@�&�@�@�r�@�Z@�(�@��;@�ƨ@�+@�-@��@�Q�@���@�S�@�"�@���@�E�@ᙚ@��`@�  @�S�@���@�j@�ƨ@ۍP@�@�^5@ى7@��@��@ם�@�S�@�@�/@Ԭ@�1@�|�@ҧ�@��@љ�@�?}@�V@���@�bN@Ͼw@�S�@��@Η�@Ͳ-@�V@�r�@�(�@�b@�\)@�ȴ@�ff@�$�@���@Ɂ@�j@Ǿw@��@�ȴ@Ə\@�=q@��@ź^@ŉ7@�G�@��@��/@ċD@�9X@�b@��m@Ý�@ÍP@�33@¸R@�=q@�@���@��9@���@�;d@��@���@�@��7@�G�@���@�Z@�b@�1@��
@��w@��F@��@�^5@�7L@�z�@�t�@�33@��@���@�ff@�x�@�?}@��@� �@�ƨ@�C�@��@�@��+@��@�7L@�Ĝ@�Z@�  @���@�l�@�
=@��@��@�V@��#@��@�%@���@��@�Q�@��@��@��@��\@�V@�-@�$�@�E�@�M�@�$�@�@�hs@�`B@�&�@�/@��D@�1'@���@�ȴ@���@�ff@�-@��@�J@��@��#@���@�X@�&�@���@���@��@�Z@�(�@���@�A�@�Q�@�9X@��;@�l�@�ȴ@��!@���@��\@�{@���@�`B@�%@��D@�9X@�1@��;@��
@�t�@��y@��R@�v�@��@��@�@�`B@��@�Ĝ@�z�@��@���@�S�@��@��y@���@�M�@�-@��-@��@��@��j@��u@�Z@�b@�ƨ@���@�l�@���@��\@��@��@��#@��h@�7L@�Ĝ@�I�@�b@��@��@�l�@�
=@���@��\@�V@�=q@��9@�~�@��j@��@u@l�@b~�@X �@N�+@HA�@?�;@7|�@0r�@)�7@#"�@O�@�\@?}@G�@(�@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	&�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	&�B	&�B	&�B	&�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	$�B	&�B	+B	.B	/B	/B	2-B	9XB	~�B
�B%�B/B-B.B49B<jB@�BC�B\)Bm�B��B��B��BoB�B�B%�B5?BB�BJ�BM�BR�BS�BN�BB�B;dB2-B�BJB��B�fB��B�wB�B�{By�BiyB_;BM�B6FBoB
��B
��B
�-B
��B
�B
iyB
R�B
B�B
'�B
+B	�mB	ĜB	��B	gmB	B�B	2-B	 �B		7B��B�B�B�TB�/B�BB�TB�ZB�ZB�TB�ZB�fB�B�B�B�yB�B�HB��B	�B	+B	9XB	B�B	I�B	R�B	XB	k�B	y�B	��B	��B	�9B	��B	��B	�B	�BB	�B	�B	��B
DB
VB
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
,B
/B
.B
+B
+B
-B
2-B
7LB
<jB
:^B
;dB
>wB
A�B
C�B
E�B
G�B
H�B
J�B
O�B
R�B
K�B
C�B
9XB
2-B
+B
&�B
!�B
"�B
&�B
(�B
0!B
.B
+B
'�B
&�B
%�B
#�B
#�B
#�B
"�B
$�B
%�B
(�B
,B
0!B
0!B
/B
1'B
1'B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
/B
.B
,B
+B
)�B
(�B
'�B
&�B
$�B
&�B
(�B
'�B
%�B
$�B
#�B
#�B
#�B
#�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
PB
	7B
1B
1B
%B
B
B
B
B
B
B
B
B
  B
  B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
%B
1B
1B
+B
%B
%B
+B
+B
%B
+B
+B
+B
+B
+B
%B
%B
+B
	7B
	7B
1B
+B
	7B
JB
JB
JB
JB
JB
PB
VB
VB
VB
PB
JB
PB
JB
JB
PB
\B
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
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
(�B
1'B
6FB
>wB
D�B
K�B
O�B
T�B
[#B
_;B
dZB
hsB
m�B
r�B
t�B
y�B
|�B
� B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	&�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	&�B	&�B	&�B	&�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	$�B	&�B	+B	.B	/B	/B	33B	;dB	�B  B.B33B33B1'B6FB>wBE�BJ�B`BBq�B��B��B��BuB�B!�B(�B9XBF�BL�BP�BXBXBS�BG�B?}B<jB�BoB��B�B�)BB�9B��B� Bm�Be`BQ�B>wB�B%B
�B
�LB
��B
�DB
p�B
W
B
I�B
33B
bB	�B	��B	�RB	x�B	H�B	:^B	/B	�B��B��B��B�B�yB�ZB�`B�fB�fB�mB�mB�B�B��B�B�B��B�ZB��B	�B	+B	:^B	C�B	K�B	S�B	YB	l�B	z�B	��B	��B	�3B	��B	�B	�#B	�NB	�B	��B	��B
VB
hB
oB
oB
�B
�B
 �B
 �B
�B
�B
�B
!�B
$�B
/B
1'B
0!B
/B
-B
.B
33B
9XB
>wB
;dB
<jB
?}B
B�B
D�B
F�B
H�B
I�B
K�B
Q�B
VB
O�B
F�B
;dB
6FB
/B
+B
"�B
"�B
&�B
(�B
49B
0!B
-B
(�B
(�B
'�B
$�B
$�B
$�B
#�B
%�B
&�B
(�B
-B
0!B
1'B
2-B
2-B
33B
1'B
0!B
2-B
33B
33B
33B
2-B
1'B
0!B
0!B
-B
,B
,B
)�B
)�B
)�B
&�B
'�B
)�B
(�B
'�B
'�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB

=B
	7B

=B
	7B
%B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
  B
B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
%B
%B
B
%B
%B
1B
	7B
1B
1B
+B
1B
1B
%B
+B
+B
+B
1B
1B
+B
+B
+B
	7B

=B
	7B
+B
	7B
JB
JB
PB
PB
PB
PB
VB
VB
\B
VB
PB
VB
PB
PB
VB
\B
bB
oB
oB
oB
oB
oB
oB
oB
uB
oB
hB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
(�B
1'B
7LB
>wB
D�B
K�B
O�B
T�B
\)B
`BB
e`B
iyB
n�B
r�B
u�B
y�B
|�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<49X<ě�<�C�<#�
<#�
<T��<�t�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250262012011312502620120113125026  AO  ARGQ                                                                        20111205113248  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125026  IP                  G�O�G�O�G�O�                