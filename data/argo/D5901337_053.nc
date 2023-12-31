CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:38Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               5A   AO  20111205113218  20190522121836  1901_5055_053                   2C  D   APEX                            2140                            040306                          846 @ԭ���	1   @ԭ���	@-������cc��-V8   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���@���A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyl�D��D�@ D�l�D�ɚD�fD�FfD���D��3D��fD�33D�vfD�� D�� D�FfD�y�D๚D���D�  D�ffD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@�  @�  A��A9��AY��Ax  A���A���A���A���A���A���A���A���B��BffBffBffB&ffB.ffB6ffB>ffBFffBN��BVffB^ffBfffBnffBvffB~ffB�33B�  B�33B�33B���B���B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]� C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,��D-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDH` DH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyS3D�  D�33D�` D���D���D�9�D���D��fD��D�&fD�i�D��3D��3D�9�D�l�D��D���D�3D�Y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�"�A�"�A��A�"�A��A�{A��A��A� �A��A�$�A�+A�+A�-A�-A�+A�-A�-A�-A�+A�(�A�+A�=qA�$�A���AͶFA�^5A���A�1'A�M�A���A��#A���Aĺ^A�$�A�p�A���A�9XA��A�/A��A���A��yA��A�1'A�x�A�ƨA�A�A��wA�A���A���A�t�A�n�A�$�A��PA�A�A�ȴA�l�A�`BA�O�A�oA�dZA��A�t�A�oA�|�A�C�A�A�A��PA�`BA��+A��A���A���A���A�;dA�jA�ZA���A�+A�"�A��A�E�A�33A��/A���A���A�z�Az�Av1As��An�Ai+Aa��AZ��AX��AV��AQ33AM��AJ��AH��AF��AD��A@��A>��A<��A;��A9A7VA4�A3��A3&�A2�9A2jA1��A2=qA2�!A2�\A2v�A21'A2A1l�A0ȴA0�A0r�A0�DA0jA0�A/�A0E�A/O�A.��A,�A+�hA*�`A*A�A)�mA(��A(JA'%A%��A%G�A%&�A$�9A$VA$5?A$1A#�A#;dA"�\A"A!|�A��A~�A�A  A"�A�\A�A��AƨA1'A�A��A�mA��A�A�AƨA%A��A�!A�uAƨA�7A33A��A�!AM�A1A�hA�A|�A&�AoA�A�RA��A�A=qA��A\)A�A=qA�wA�Ap�A?}A�A�RA�A�DA�TA�A�-A��A?}A
��A
�uA	�-A	C�A�yA�\A5?AG�A��A�9AM�A�A��AG�AȴAv�A{A��A7LA��A�9Ar�A(�A�TA�wAp�A �yA �9A E�A @�dZ@��@�-@��@��D@���@�l�@�"�@���@���@�@�7L@�I�@�C�@�-@��^@�p�@��`@�@�$�@�X@�V@��@�1'@�dZ@�n�@��-@�j@�9X@���@�o@�V@��@��@�?}@�j@�j@�b@�|�@�h@��`@�r�@�w@���@��@���@��@��@��@�dZ@���@��@��/@��;@�C�@���@ڗ�@�V@��@�X@�z�@��@�t�@֟�@�@�`B@Ԭ@�9X@ӝ�@�
=@ҏ\@�-@ѩ�@�V@��/@�Z@���@�dZ@�
=@θR@Η�@�n�@�=q@�@̼j@˶F@�\)@�;d@��H@�^5@���@ɩ�@�p�@��@�I�@���@Ǯ@�dZ@�
=@�ff@�J@őh@�&�@ċD@�I�@�A�@���@���@���@���@�G�@���@�Q�@�|�@��@�^5@���@�hs@�G�@���@���@�bN@�1@���@�t�@�"�@��@�5?@�J@��h@�%@�z�@�bN@�Z@�A�@���@�33@�$�@��h@���@��@�r�@�bN@� �@��@�K�@�o@�ȴ@�v�@�5?@���@�O�@��@��D@�Z@�1'@��@��w@��P@��@��@�p�@�&�@���@�/@��D@��@��@�ȴ@��!@�ȴ@��+@�=q@�-@�n�@�=q@�M�@�$�@�{@�{@�$�@�@�O�@�%@��/@��@�Z@���@���@�C�@�@���@�v�@��@�@���@�G�@���@�z�@�bN@�9X@�l�@���@��+@�{@�`B@���@�z�@�9X@�b@���@��P@�t�@�\)@�\)@�"�@��\@�$�@��@���@���@�?}@��9@�r�@�1'@�  @��@�t�@�"�@�
=@���@��\@�{@��#@�x�@�7L@��/@���@�9X@�dZ@�"�@�o@��R@�^5@��@��-@�hs@�/@��`@��@��@�Z@� �@���@��P@�p�@�dZ@}/@s33@kC�@b�\@YX@O�;@HbN@?K�@7��@.�y@)%@"�@V@hs@`B@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�"�A�"�A�"�A��A�"�A��A�{A��A��A� �A��A�$�A�+A�+A�-A�-A�+A�-A�-A�-A�+A�(�A�+A�=qA�$�A���AͶFA�^5A���A�1'A�M�A���A��#A���Aĺ^A�$�A�p�A���A�9XA��A�/A��A���A��yA��A�1'A�x�A�ƨA�A�A��wA�A���A���A�t�A�n�A�$�A��PA�A�A�ȴA�l�A�`BA�O�A�oA�dZA��A�t�A�oA�|�A�C�A�A�A��PA�`BA��+A��A���A���A���A�;dA�jA�ZA���A�+A�"�A��A�E�A�33A��/A���A���A�z�Az�Av1As��An�Ai+Aa��AZ��AX��AV��AQ33AM��AJ��AH��AF��AD��A@��A>��A<��A;��A9A7VA4�A3��A3&�A2�9A2jA1��A2=qA2�!A2�\A2v�A21'A2A1l�A0ȴA0�A0r�A0�DA0jA0�A/�A0E�A/O�A.��A,�A+�hA*�`A*A�A)�mA(��A(JA'%A%��A%G�A%&�A$�9A$VA$5?A$1A#�A#;dA"�\A"A!|�A��A~�A�A  A"�A�\A�A��AƨA1'A�A��A�mA��A�A�AƨA%A��A�!A�uAƨA�7A33A��A�!AM�A1A�hA�A|�A&�AoA�A�RA��A�A=qA��A\)A�A=qA�wA�Ap�A?}A�A�RA�A�DA�TA�A�-A��A?}A
��A
�uA	�-A	C�A�yA�\A5?AG�A��A�9AM�A�A��AG�AȴAv�A{A��A7LA��A�9Ar�A(�A�TA�wAp�A �yA �9A E�A @�dZ@��@�-@��@��D@���@�l�@�"�@���@���@�@�7L@�I�@�C�@�-@��^@�p�@��`@�@�$�@�X@�V@��@�1'@�dZ@�n�@��-@�j@�9X@���@�o@�V@��@��@�?}@�j@�j@�b@�|�@�h@��`@�r�@�w@���@��@���@��@��@��@�dZ@���@��@��/@��;@�C�@���@ڗ�@�V@��@�X@�z�@��@�t�@֟�@�@�`B@Ԭ@�9X@ӝ�@�
=@ҏ\@�-@ѩ�@�V@��/@�Z@���@�dZ@�
=@θR@Η�@�n�@�=q@�@̼j@˶F@�\)@�;d@��H@�^5@���@ɩ�@�p�@��@�I�@���@Ǯ@�dZ@�
=@�ff@�J@őh@�&�@ċD@�I�@�A�@���@���@���@���@�G�@���@�Q�@�|�@��@�^5@���@�hs@�G�@���@���@�bN@�1@���@�t�@�"�@��@�5?@�J@��h@�%@�z�@�bN@�Z@�A�@���@�33@�$�@��h@���@��@�r�@�bN@� �@��@�K�@�o@�ȴ@�v�@�5?@���@�O�@��@��D@�Z@�1'@��@��w@��P@��@��@�p�@�&�@���@�/@��D@��@��@�ȴ@��!@�ȴ@��+@�=q@�-@�n�@�=q@�M�@�$�@�{@�{@�$�@�@�O�@�%@��/@��@�Z@���@���@�C�@�@���@�v�@��@�@���@�G�@���@�z�@�bN@�9X@�l�@���@��+@�{@�`B@���@�z�@�9X@�b@���@��P@�t�@�\)@�\)@�"�@��\@�$�@��@���@���@�?}@��9@�r�@�1'@�  @��@�t�@�"�@�
=@���@��\@�{@��#@�x�@�7L@��/@���@�9X@�dZ@�"�@�o@��R@�^5@��@��-@�hs@�/@��`@��@��@�Z@� �@���@��P@�p�@�dZ@}/@s33@kC�@b�\@YX@O�;@HbN@?K�@7��@.�y@)%@"�@V@hs@`B@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	+B	+B	)�B	(�B	(�B	'�B	&�B	&�B	&�B	(�B	&�B	(�B	)�B	)�B	+B	+B	)�B	)�B	)�B	)�B	)�B	)�B	-B	s�BB]/B�+B�oB��B�hB�%B�B`BB"�B�B1'B1'B9XB�oB��B��B�}BB�BJB�B�BB�BB�B��B� BG�B6FBS�Bw�B��B�B�?B�^B�wBƨBŢBȴB��B��BÖBĜB��B��B�HB�5B��BB�LB�?B��B�DBQ�B&�BB
�HB
�XB
��B
�oB
k�B
H�B
+B
%B	�B	�dB	��B	� B	ZB	,B	PB	B��B�;B��BɺBÖBB�}B�qB�wB��B��B��BȴBB��BÖBɺB��B��B�B	JB	�B	%�B	33B	>wB	R�B	cTB	k�B	o�B	r�B	y�B	�B	��B	��B	B	��B	��B	�B	��B	�B	�/B	�HB	�TB	�sB	�B	�B	�B	�B	�B	�yB	�mB	�`B	�ZB	�ZB	�TB	�NB	�HB	�HB	�BB	�5B	�;B	�5B	�HB	�TB	�B
B
B	��B	�B	��B
DB
+B
B
B
1B

=B
JB
	7B
bB
PB
VB
PB
DB
DB
PB
hB
�B
�B
�B
�B
 �B
&�B
(�B
(�B
+B
)�B
)�B
&�B
%�B
$�B
&�B
,B
,B
,B
-B
-B
+B
-B
.B
-B
+B
(�B
)�B
&�B
%�B
$�B
$�B
"�B
 �B
 �B
�B
�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
uB
oB
oB
hB
oB
hB
hB
bB
\B
VB
PB
DB
DB

=B
	7B
1B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
%B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
DB
DB
DB
JB
JB
JB
JB
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
bB
bB
bB
bB
bB
bB
\B
\B
bB
bB
\B
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
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
,B
2-B
8RB
>wB
D�B
I�B
N�B
R�B
[#B
aHB
ffB
jB
n�B
r�B
u�B
x�B
}�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	+B	+B	)�B	(�B	(�B	'�B	&�B	&�B	&�B	(�B	&�B	(�B	)�B	)�B	+B	+B	)�B	)�B	)�B	)�B	)�B	)�B	-B	t�BB^5B�1B�uB��B�{B�+B�%Bm�B&�B�B5?B5?B<jB�{B��B�B��BȴB��B\B�B�B%B�BDB�B�mB�PBS�BB�BW
By�B��B�!B�?B�^B�}BɺBȴBɺB��B��BĜBĜB��B�B�fB�HB�)BȴB�jB�wB��B��B`BB2-B\B
�B
��B
�B
��B
u�B
Q�B
5?B
�B	�NB	��B	�!B	�VB	m�B	?}B	uB	1B	B�fB�B��BȴBǮBɺBBĜBĜB��B��B��BŢBÖBŢB��B��B��B�B	PB	�B	&�B	49B	@�B	T�B	dZB	k�B	o�B	s�B	z�B	�B	��B	B	ĜB	ǮB	��B	�B	��B	�B	�BB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�fB	�fB	�`B	�mB	�fB	�`B	�`B	�HB	�HB	�BB	�NB	�ZB	�B
+B
B	��B	��B	��B
\B
DB
B
B
1B
DB
VB

=B
hB
VB
\B
VB
JB
JB
PB
hB
�B
�B
�B
�B
 �B
'�B
)�B
)�B
-B
+B
-B
(�B
&�B
$�B
'�B
-B
-B
,B
.B
/B
,B
-B
.B
.B
-B
)�B
-B
'�B
&�B
%�B
%�B
%�B
!�B
!�B
 �B
 �B
"�B
"�B
"�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
uB
uB
oB
uB
oB
oB
oB
bB
bB
\B
PB
JB
DB

=B

=B
1B
+B
%B
%B
%B
%B
+B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
+B
+B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
%B
%B
B
B
B
B
B
B
+B
%B
%B
B
B
B
B
B
B
B
1B
	7B
JB
DB
DB
JB
PB
PB
PB
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
bB
bB
hB
hB
hB
bB
hB
bB
bB
bB
hB
bB
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
oB
hB
oB
oB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
,B
2-B
8RB
>wB
D�B
I�B
N�B
S�B
[#B
aHB
ffB
jB
n�B
r�B
u�B
y�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<T��<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<e`B<49X<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<49X<e`B<���<���<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250262012011312502620120113125026  AO  ARGQ                                                                        20111205113218  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113218  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125026  IP                  G�O�G�O�G�O�                