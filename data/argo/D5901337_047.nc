CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:36Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               /A   AO  20111205113125  20190522121836  1901_5055_047                   2C  D   APEX                            2140                            040306                          846 @Ԟ)\ 1   @Ԟ)��@/bM����c Z�11   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@ffBH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dy�fD�	�D�S3D�ffD��3D��D�33D�vfD��3D���D�&fD�3D�� D���D�,�Dڃ3D�fD��3D�fD�P D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A33A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.  B6ffB>��BFffBN��BV��B^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�ffB�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%� C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQl�DQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDq` Dq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�3Dyl�D���D�FfD�Y�D��fD�  D�&fD�i�D��fD�� D��D�fDǳ3D�� D�  D�vfDਗ਼D��fD�	�D�C3D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�p�A�p�A�p�A�p�A�r�A�x�A�x�A�v�A�x�A�x�A�x�A�r�A�v�A�x�A�z�A�|�A�|�A�v�A�t�A�x�A�x�A�r�A�~�A�~�A�v�A�hsA�O�A�ffA�=qAϩ�A��Aδ9A��
A�x�A˓uA�?}A���A��`A��A�z�A��9A�n�A��A�
=A���A��A���A�33A��A��FA�-A���A�ffA�ZA�7LA��!A�A�K�A�Q�A�XA�hsA��TA�-A�%A���A�{A�C�A�1A�x�A��A���A���A�x�A�r�A���A�\)A�&�A�"�A���A��A��FA��\A���A��mA�+A�5?A�|�A���A��DA}XAw�Ar9XAk7LAeC�Abz�AbI�A_/AZ�\AXn�AU/AR{AP{AN(�ALJAJ��AIK�AG��AE|�AC�AB��AB�\AA�TAAG�A@ĜA@��A@ZA?�A@  A@�A?�hA>��A=�
A<��A<n�A<�jA<��A<-A:��A:  A9?}A8�jA8r�A8ffA8�+A8�uA7�A6�HA6�+A5�wA57LA5;dA5A4��A4�+A3�#A3�A2VA1��A1�A1\)A1A0�A0=qA.ĜA.=qA-
=A,M�A+dZA*�RA*��A*��A*�A*{A)�wA)`BA(ȴA't�A&Q�A%�7A$��A$ffA$  A#�hA#��A#��A#?}A"��A"jA"9XA" �A!��A!t�A �A�TA�RAJA��AC�A��A��Av�A�^A�jA1A�^A�A�AȴA$�AG�A�+A��A�7A�`AJAdZA�HA�HAQ�A��A-A
=A�DA5?A �A��Al�A~�A��A%A
�DA
  A	�7Av�A�
AG�AXAK�A�A"�A%AjA5?A�FA�PA\)A+A��A�yA�A�+AffA�#A�AĜA��A�PA33A �A �RA �DA b@�ȴ@��#@��`@�z�@��@��;@�ff@��#@��^@���@��-@��9@��!@�=q@��@�x�@�9X@�t�@��@�v�@�@��#@�O�@�1@�w@�t�@�o@�~�@�J@���@���@��-@�x�@���@�o@�ff@�hs@��/@�D@�F@�ȴ@��@��@�z�@���@�K�@�n�@�^@�?}@���@�9X@��;@�l�@���@ާ�@�V@ݑh@���@���@�C�@��@ڰ!@���@��`@�Ĝ@�b@�33@֏\@�n�@�^5@�5?@��@�7L@ԋD@��@Ӆ@���@��@�7L@�Z@ϝ�@�"�@���@�J@͑h@���@�b@�33@ʟ�@�~�@�n�@�-@��#@ɺ^@ɉ7@���@ț�@� �@�;d@���@�ȴ@�v�@�^5@�{@�hs@�V@�r�@�ƨ@�K�@�
=@�~�@�5?@��^@�?}@��D@���@���@�S�@��y@�=q@��@��-@�&�@�j@��m@�K�@��y@���@��@�p�@�V@��@���@��@��@�1@��@�C�@�
=@�ȴ@�5?@��-@�X@��`@�1'@���@��@���@�v�@�-@��7@��@��@�r�@���@�\)@�"�@��H@���@���@�-@���@�?}@��@�bN@��@�ƨ@���@�@���@�v�@�^5@��@�@���@��7@��/@�Z@�9X@� �@�  @���@�K�@�+@���@�^5@���@���@�X@��@�j@�1'@��@�|�@���@�ȴ@���@�E�@��@��h@�V@��j@�r�@��@��F@�S�@�C�@�@���@��+@�^5@�5?@��@�@�p�@��@���@��u@�Z@�9X@�1@��@�ƨ@���@�l�@��@���@��+@�v�@�^5@�{@��@�?}@��`@���@�Z@�1'@��@��w@�l�@�E�@��u@��7@��@~E�@u?}@h�u@^{@Tz�@Mp�@F�@Ahs@<(�@5`B@/�@*=q@$�@��@-@@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�p�A�p�A�p�A�p�A�r�A�x�A�x�A�v�A�x�A�x�A�x�A�r�A�v�A�x�A�z�A�|�A�|�A�v�A�t�A�x�A�x�A�r�A�~�A�~�A�v�A�hsA�O�A�ffA�=qAϩ�A��Aδ9A��
A�x�A˓uA�?}A���A��`A��A�z�A��9A�n�A��A�
=A���A��A���A�33A��A��FA�-A���A�ffA�ZA�7LA��!A�A�K�A�Q�A�XA�hsA��TA�-A�%A���A�{A�C�A�1A�x�A��A���A���A�x�A�r�A���A�\)A�&�A�"�A���A��A��FA��\A���A��mA�+A�5?A�|�A���A��DA}XAw�Ar9XAk7LAeC�Abz�AbI�A_/AZ�\AXn�AU/AR{AP{AN(�ALJAJ��AIK�AG��AE|�AC�AB��AB�\AA�TAAG�A@ĜA@��A@ZA?�A@  A@�A?�hA>��A=�
A<��A<n�A<�jA<��A<-A:��A:  A9?}A8�jA8r�A8ffA8�+A8�uA7�A6�HA6�+A5�wA57LA5;dA5A4��A4�+A3�#A3�A2VA1��A1�A1\)A1A0�A0=qA.ĜA.=qA-
=A,M�A+dZA*�RA*��A*��A*�A*{A)�wA)`BA(ȴA't�A&Q�A%�7A$��A$ffA$  A#�hA#��A#��A#?}A"��A"jA"9XA" �A!��A!t�A �A�TA�RAJA��AC�A��A��Av�A�^A�jA1A�^A�A�AȴA$�AG�A�+A��A�7A�`AJAdZA�HA�HAQ�A��A-A
=A�DA5?A �A��Al�A~�A��A%A
�DA
  A	�7Av�A�
AG�AXAK�A�A"�A%AjA5?A�FA�PA\)A+A��A�yA�A�+AffA�#A�AĜA��A�PA33A �A �RA �DA b@�ȴ@��#@��`@�z�@��@��;@�ff@��#@��^@���@��-@��9@��!@�=q@��@�x�@�9X@�t�@��@�v�@�@��#@�O�@�1@�w@�t�@�o@�~�@�J@���@���@��-@�x�@���@�o@�ff@�hs@��/@�D@�F@�ȴ@��@��@�z�@���@�K�@�n�@�^@�?}@���@�9X@��;@�l�@���@ާ�@�V@ݑh@���@���@�C�@��@ڰ!@���@��`@�Ĝ@�b@�33@֏\@�n�@�^5@�5?@��@�7L@ԋD@��@Ӆ@���@��@�7L@�Z@ϝ�@�"�@���@�J@͑h@���@�b@�33@ʟ�@�~�@�n�@�-@��#@ɺ^@ɉ7@���@ț�@� �@�;d@���@�ȴ@�v�@�^5@�{@�hs@�V@�r�@�ƨ@�K�@�
=@�~�@�5?@��^@�?}@��D@���@���@�S�@��y@�=q@��@��-@�&�@�j@��m@�K�@��y@���@��@�p�@�V@��@���@��@��@�1@��@�C�@�
=@�ȴ@�5?@��-@�X@��`@�1'@���@��@���@�v�@�-@��7@��@��@�r�@���@�\)@�"�@��H@���@���@�-@���@�?}@��@�bN@��@�ƨ@���@�@���@�v�@�^5@��@�@���@��7@��/@�Z@�9X@� �@�  @���@�K�@�+@���@�^5@���@���@�X@��@�j@�1'@��@�|�@���@�ȴ@���@�E�@��@��h@�V@��j@�r�@��@��F@�S�@�C�@�@���@��+@�^5@�5?@��@�@�p�@��@���@��u@�Z@�9X@�1@��@�ƨ@���@�l�@��@���@��+@�v�@�^5@�{@��@�?}@��`@���@�Z@�1'@��@��w@�l�@�E�@��u@��7@��@~E�@u?}@h�u@^{@Tz�@Mp�@F�@Ahs@<(�@5`B@/�@*=q@$�@��@-@@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�qB
�B
�B
�sB
�ZB
�NB
�HB
�sB
�B
��B
�!B
�}BB'�B8RBH�B�B�BB  BoB{B�BA�B[#B_;B^5BW
BXBO�B9XBPB�B�`B��BǮB�}B�dB�'B��B��B��B��B�\B��B��B�VBn�BaHBXB,B
��B
��B
��B
v�B
YB
;dB
7LB
7LB
%�B
+B	�B	�?B	�{B	v�B	T�B	+B	uB	JB	1B	B	%B		7B	B	JB	�B	"�B	-B	0!B	C�B	L�B	dZB	u�B	�1B	��B	�?B	ÖB	��B	�/B	�)B	�#B	�HB	�B	��B
B
B
B
�B
2-B
H�B
O�B
G�B
E�B
D�B
L�B
P�B
T�B
XB
cTB
bNB
_;B
_;B
cTB
jB
u�B
w�B
}�B
�B
z�B
u�B
p�B
r�B
~�B
�B
� B
� B
|�B
w�B
s�B
l�B
jB
ffB
aHB
dZB
m�B
q�B
t�B
s�B
q�B
l�B
e`B
]/B
W
B
S�B
P�B
M�B
L�B
S�B
XB
[#B
_;B
aHB
dZB
dZB
cTB
cTB
aHB
\)B
Q�B
M�B
L�B
L�B
K�B
J�B
H�B
D�B
>wB
;dB
:^B
:^B
:^B
8RB
2-B
,B
)�B
'�B
&�B
"�B
 �B
�B
#�B
(�B
(�B
(�B
&�B
"�B
#�B
"�B
"�B
"�B
�B
�B
�B
�B
�B
oB
\B

=B
DB

=B
VB
hB
hB
�B
�B
�B
{B
uB
{B
�B
�B
�B
�B
�B
�B
uB
bB
\B
bB
\B
\B
\B
\B
\B
PB
DB
	7B

=B
	7B

=B

=B

=B
+B
B
B
B
1B
+B
B
B
B
B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
	7B
	7B

=B

=B

=B
	7B
	7B
	7B
	7B
	7B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
oB
uB
uB
{B
{B
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
(�B
/B
33B
7LB
>wB
D�B
K�B
P�B
T�B
YB
^5B
bNB
e`B
iyB
n�B
r�B
v�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ĜB
�B
�B
�B
�mB
�mB
�ZB
�B
�5B
ŢB
�-B
��B%B0!B:^BH�B�B�mBB{B�B�BH�B]/B`BBcTB\)B[#BR�BA�B�B�B�B�B��BÖB��B�XB��B�B�B��B�oB��B�'B��Br�BcTBdZB<jB
��B
�sB
��B
�B
iyB
A�B
<jB
<jB
.B
hB	�NB	��B	��B	�B	dZB	8RB	�B	PB	bB	JB	
=B	VB	+B	bB	�B	'�B	1'B	49B	G�B	S�B	iyB	x�B	�=B	��B	�LB	ŢB	��B	�5B	�5B	�#B	�HB	�B	��B
%B
%B
B
�B
2-B
K�B
T�B
I�B
G�B
F�B
M�B
P�B
T�B
XB
e`B
dZB
`BB
aHB
e`B
jB
v�B
x�B
~�B
�B
|�B
w�B
r�B
r�B
� B
�B
�B
�B
�B
y�B
w�B
o�B
m�B
hsB
aHB
dZB
n�B
s�B
u�B
t�B
s�B
p�B
hsB
`BB
YB
VB
Q�B
N�B
L�B
S�B
YB
\)B
`BB
bNB
dZB
e`B
dZB
e`B
dZB
_;B
S�B
N�B
M�B
M�B
L�B
K�B
J�B
G�B
@�B
<jB
;dB
;dB
;dB
:^B
5?B
.B
,B
(�B
(�B
%�B
"�B
!�B
#�B
+B
+B
-B
)�B
$�B
$�B
"�B
#�B
$�B
!�B
�B
�B
�B
�B
{B
oB
JB
PB

=B
VB
oB
hB
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
uB
hB
bB
bB
bB
bB
VB
PB

=B
DB

=B
DB
DB
JB
1B
B
B
B

=B

=B
%B
B
B
B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
%B
+B
+B
+B
1B
1B
1B
+B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB

=B
	7B
	7B
	7B

=B
	7B
1B

=B
DB
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
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
{B
uB
�B
{B
{B
�B
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
)�B
/B
49B
8RB
>wB
E�B
K�B
P�B
VB
ZB
^5B
bNB
ffB
iyB
n�B
r�B
v�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�o<#�
<���<D��<e`B<�o<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<49X<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250242012011312502420120113125024  AO  ARGQ                                                                        20111205113125  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113125  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125024  IP                  G�O�G�O�G�O�                