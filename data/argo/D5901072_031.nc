CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:48Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143812  20190522121828  1728_5048_031                   2C  D   APEX                            2142                            040306                          846 @�w�F?�1   @�w�/h@@5-�hr�!�cqV�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C�C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy�fD�3D�C3D��3D�� D�ٚD��D�i�D��fD���D�  D�p DǖfD�� D�&fD�)�D��3D�ٚD�	�D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  @�33A��A9��AY��Ay��A���A���A���A���A���Aݙ�A홚A���BffB  BffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33C��C��C�3C��C	� C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU�3CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fD ffD � D!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&l�D&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4l�D4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDk` Dk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwY�Dy��D��fD�6fD��fD��3D���D�  D�\�D���D�� D�3D�c3Dǉ�D��3D��D��D�fD���D���D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�VA�S�A�^5A�bNA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�l�A�n�A�l�A�M�A��A��
A�?}A��`A�~�A���A��A�7LA���A��A�I�A�"�A���A���A�A�A�A��A��TA���A���A�-A��A���A���A�bNA��-A��A���A���A�~�A�7LA�$�A�9XA��wA�%A��
A�-A���A��A�%A�ZA���A���A���A���A�|�A��-A�I�A��A��A��A�Q�A��A�K�A��+A���A�33A�~�A���A���A� �A��^A���A���A�C�A���A�ƨA��DA��A�XA��RA��A�?}A�%A���A�I�A�ĜA�ĜA��wA��!A�~�A�ffA���A��yA~�yA}/A{�-Ay33Ax�!AxM�AwAr�uAm+Aj�uAi�Ai�PAh��Ag�Af��AedZAc|�A`ȴA\ȴAY�AX�\AW�hAW�AV-AT�/AS33AP��AOS�ANr�AM�hAL  AK
=AI�TAH��AH�AGx�AEAD�HAC7LAB9XAA��AAhsA@��A?��A?/A>I�A=�A=O�A<�A:ȴA9��A8�jA8jA7�A57LA4�DA3��A3?}A2~�A1&�A0r�A/�A/
=A.v�A-�FA,�+A+7LA)�mA)C�A)�A(�A(�jA(Q�A'�mA'�FA'�A&�yA%|�A$ĜA#�A"�9A"{A!�
A!�FA!�A!7LA ��A�-AhsA��Av�A��A��A/AA�A�jAv�A\)A��A-A�hAt�A�AȴA�uA=qAbNA��A�A
-A �A�\Ax�A��A�A?}A��A9XAQ�Ax�A z�@�+@���@���@���@���@��@���@�@���@��@���@�+@���@��@�@�b@�@�@��@���@ܓu@�M�@�
=@�7L@�?}@�~�@��@���@�S�@�l�@���@�j@�A�@�$�@���@�j@��
@�K�@���@���@��`@�  @���@��@���@��T@�?}@�j@�  @��@�ƨ@��P@�dZ@��@�n�@��@�(�@��m@�v�@��`@�9X@���@�dZ@�+@�
=@�@��@�ȴ@��+@�v�@�n�@�^5@��#@��/@��;@�o@�@�p�@�G�@�/@��@�&�@�7L@�`B@��h@��^@�@���@��#@��#@���@��-@��7@�G�@���@��@��j@���@��`@��@�/@�/@�&�@���@�V@�/@�G�@���@���@���@�`B@�/@��@�%@���@�%@�%@���@���@��D@�K�@�ff@�p�@���@�r�@�(�@��;@�K�@��@��R@�v�@�E�@��@���@�O�@��y@� �@�Ĝ@��@��H@��`@�&�@�&�@��`@��D@�l�@�M�@�J@�x�@�Z@��@���@��@�ff@��@�C�@��y@�ȴ@��!@���@���@�v�@�/@�(�@��F@�33@��+@��@��T@�@�p�@�`B@�?}@���@��m@�K�@�"�@��@��H@�~�@���@��7@�&�@���@���@�Ĝ@���@��@�j@�I�@�b@�K�@���@��@�ȴ@��R@��\@��+@�v�@�=q@���@�X@��`@�A�@�I�@�I�@�I�@�I�@�Q�@�9X@�b@��;@���@�l�@�K�@�;d@�33@�+@�+@��@�o@�o@��@��H@��y@�o@�\)@�l�@�t�@���@��m@��@�Z@���@��@���@���@�b@�dZ@�;d@�o@��H@��!@�v�@�$�@���@��@���@���@�7L@�V@���@��@�z�@���@�ƨ@�l�@�@��y@�ȴ@�V@�`B@���@�Z@�1'@�(�@�b@��@K�@~ff@}�-@}�@|�/@sS�@jJ@`1'@X��@R-@L�/@G�@A7L@;�
@41@/��@*J@'�@!�#@��@V@1'@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XA�VA�S�A�^5A�bNA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�l�A�n�A�l�A�M�A��A��
A�?}A��`A�~�A���A��A�7LA���A��A�I�A�"�A���A���A�A�A�A��A��TA���A���A�-A��A���A���A�bNA��-A��A���A���A�~�A�7LA�$�A�9XA��wA�%A��
A�-A���A��A�%A�ZA���A���A���A���A�|�A��-A�I�A��A��A��A�Q�A��A�K�A��+A���A�33A�~�A���A���A� �A��^A���A���A�C�A���A�ƨA��DA��A�XA��RA��A�?}A�%A���A�I�A�ĜA�ĜA��wA��!A�~�A�ffA���A��yA~�yA}/A{�-Ay33Ax�!AxM�AwAr�uAm+Aj�uAi�Ai�PAh��Ag�Af��AedZAc|�A`ȴA\ȴAY�AX�\AW�hAW�AV-AT�/AS33AP��AOS�ANr�AM�hAL  AK
=AI�TAH��AH�AGx�AEAD�HAC7LAB9XAA��AAhsA@��A?��A?/A>I�A=�A=O�A<�A:ȴA9��A8�jA8jA7�A57LA4�DA3��A3?}A2~�A1&�A0r�A/�A/
=A.v�A-�FA,�+A+7LA)�mA)C�A)�A(�A(�jA(Q�A'�mA'�FA'�A&�yA%|�A$ĜA#�A"�9A"{A!�
A!�FA!�A!7LA ��A�-AhsA��Av�A��A��A/AA�A�jAv�A\)A��A-A�hAt�A�AȴA�uA=qAbNA��A�A
-A �A�\Ax�A��A�A?}A��A9XAQ�Ax�A z�@�+@���@���@���@���@��@���@�@���@��@���@�+@���@��@�@�b@�@�@��@���@ܓu@�M�@�
=@�7L@�?}@�~�@��@���@�S�@�l�@���@�j@�A�@�$�@���@�j@��
@�K�@���@���@��`@�  @���@��@���@��T@�?}@�j@�  @��@�ƨ@��P@�dZ@��@�n�@��@�(�@��m@�v�@��`@�9X@���@�dZ@�+@�
=@�@��@�ȴ@��+@�v�@�n�@�^5@��#@��/@��;@�o@�@�p�@�G�@�/@��@�&�@�7L@�`B@��h@��^@�@���@��#@��#@���@��-@��7@�G�@���@��@��j@���@��`@��@�/@�/@�&�@���@�V@�/@�G�@���@���@���@�`B@�/@��@�%@���@�%@�%@���@���@��D@�K�@�ff@�p�@���@�r�@�(�@��;@�K�@��@��R@�v�@�E�@��@���@�O�@��y@� �@�Ĝ@��@��H@��`@�&�@�&�@��`@��D@�l�@�M�@�J@�x�@�Z@��@���@��@�ff@��@�C�@��y@�ȴ@��!@���@���@�v�@�/@�(�@��F@�33@��+@��@��T@�@�p�@�`B@�?}@���@��m@�K�@�"�@��@��H@�~�@���@��7@�&�@���@���@�Ĝ@���@��@�j@�I�@�b@�K�@���@��@�ȴ@��R@��\@��+@�v�@�=q@���@�X@��`@�A�@�I�@�I�@�I�@�I�@�Q�@�9X@�b@��;@���@�l�@�K�@�;d@�33@�+@�+@��@�o@�o@��@��H@��y@�o@�\)@�l�@�t�@���@��m@��@�Z@���@��@���@���@�b@�dZ@�;d@�o@��H@��!@�v�@�$�@���@��@���@���@�7L@�V@���@��@�z�@���@�ƨ@�l�@�@��y@�ȴ@�V@�`B@���@�Z@�1'@�(�@�b@��@K�@~ff@}�-@}�@|�/@sS�@jJ@`1'@X��@R-@L�/@G�@A7L@;�
@41@/��@*J@'�@!�#@��@V@1'@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�JB��B�jB��B��B%B{B2-BB�BK�BO�BM�BS�BW
BXBZBcTBu�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�PBm�Bm�Bo�Bx�B~�BW
BM�BVB\)BXB^5B\)BO�B<jB#�B�B#�B'�B�B\B��B�B�TB�/B�
B��B��B�LB�FB�LB��B��B�DB\)BP�B?}B5?B�B%B
�B
�`B
��B
�3B
��B
�B
s�B
hsB
^5B
C�B
,B
�B	��B	�B	�B	�NB	�3B	�VB	}�B	� B	z�B	t�B	t�B	gmB	cTB	ZB	J�B	B�B	)�B	%�B	)�B	'�B	.B	'�B	�B	uB	VB	DB	1B	B	B��B��B��B��B��B�B�B�fB�mB�fB�ZB�5B�;B�B�)B�B�B�B�
B�)B��B��B�B��BƨBÖBB�dB�jB�qB�XB�9B�RB�LB�?B�-B�B�B�B�B��B�B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�uB��B�uB�JB�B�+B�DB�Bx�Bu�Bs�Br�Bx�B�bB��B��B��B�PB�B�DB�\B��B�B�B�3B�?B�3B�-B�9B�'B�B�B�!B�B��B��B�BjB[#BE�BG�BR�BZBH�B<jB1'B,B,B.B-B-B-B.B/B1'B49B8RB9XB9XB9XB9XB:^B;dB@�B@�BA�BB�BC�BD�BD�BD�BP�BR�BN�BS�BXB`BBdZBaHBdZBgmBhsBjBm�Bw�By�Bz�Bz�B}�Bz�Bz�B{�B�B~�B�B�B�%B�+B�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B�B�?B�LB�^B�wB��BBŢBȴB��B��B��B�B�B�BB�B�B�B�B�B��B��B��B��B��B	\B	uB	�B	bB	bB	hB	hB	hB	hB	uB	{B	�B	�B	�B	"�B	0!B	;dB	B�B	F�B	O�B	e`B	iyB	k�B	l�B	l�B	m�B	n�B	n�B	l�B	r�B	m�B	iyB	jB	l�B	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	k�B	n�B	n�B	o�B	p�B	s�B	v�B	}�B	�B	�1B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�FB	�?B	�RB	�RB	�RB	�RB	�RB	�RB	�^B	�jB	�wB	B	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�TB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
 �B
(�B
1'B
49B
9XB
>wB
H�B
J�B
O�B
T�B
ZB
\)B
dZB
m�B
r�B
v�B
{�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�DB�VB��B��B�B��B	7B�B5?BE�BN�BS�BN�BT�BXB[#B]/Be`Bx�B��B��B��B��B��B�B�B�B�'B�B��B��B��B��B��B��B��B��B��Bn�Bm�Bo�B�B�1B\)BO�BZB_;B]/BaHB`BBT�BA�B(�B�B'�B-B"�B�B��B�B�`B�BB�B��B�B�XB�XB�^B�B��B�{B_;BT�BB�B;dB"�BPB
��B
�B
�HB
��B
��B
�=B
x�B
o�B
hsB
I�B
2-B
�B	��B	��B	�B	�B	��B	��B	� B	�B	|�B	v�B	v�B	jB	gmB	_;B	Q�B	H�B	-B	(�B	,B	+B	2-B	-B	&�B	�B	hB	\B	VB		7B	+B	B��B��B��B��B��B�B�sB�B�yB�mB�HB�NB�/B�5B�;B�/B�B�B�/B��B��B�)B��BǮBŢBŢB�qB�qB�}B�dB�FB�dB�dB�XB�9B�B�B�B�B�B�B�B�B�!B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B�\B�+B�DB�VB�%Bz�Bx�Bu�Bt�Bx�B�uB��B��B��B�hB�1B�DB�\B��B�B�B�?B�FB�?B�?B�XB�3B�'B�3B�-B�'B�B��B�PBp�BdZBE�BF�BR�BaHBN�BC�B5?B-B-B2-B/B.B.B/B1'B33B6FB:^B;dB:^B:^B;dB<jB=qBA�BA�BB�BC�BD�BE�BE�BF�BR�BS�BP�BW
BYBaHBe`BbNBe`BgmBhsBk�Bn�Bw�By�Bz�B{�B� B|�B|�B}�B�B� B�B�B�%B�+B�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B�B�?B�LB�^B�wB��BBƨBȴB��B��B��B�B�B�BB�B�B�B�B�B��B��B��B��B��B	hB	�B	�B	hB	hB	oB	oB	oB	oB	{B	�B	�B	�B	�B	 �B	.B	:^B	B�B	D�B	L�B	e`B	iyB	l�B	m�B	n�B	o�B	o�B	o�B	n�B	u�B	p�B	k�B	m�B	p�B	hsB	hsB	gmB	gmB	gmB	gmB	hsB	iyB	m�B	o�B	o�B	p�B	q�B	t�B	v�B	~�B	�B	�7B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�LB	�LB	�FB	�RB	�RB	�RB	�RB	�RB	�RB	�dB	�qB	�}B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�/B	�NB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
 �B
(�B
1'B
5?B
9XB
?}B
H�B
J�B
P�B
T�B
ZB
]/B
dZB
m�B
r�B
v�B
{�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452032012011014520320120110145203  AO  ARGQ                                                                        20111130143812  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143812  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145203  IP                  G�O�G�O�G�O�                