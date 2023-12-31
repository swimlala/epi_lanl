CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:54Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               9A   AO  20111130144100  20190522121829  1728_5048_057                   2C  D   APEX                            2142                            040306                          846 @ԸuB@	1   @Ըu��?�@5NV�u�c�n��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy� D�  D�0 D�y�D��3D���D�9�D�#3D�ɚD��fD�0 D���Dǹ�D���D��D�` D��3D��3D�  D�6fD�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@`  @�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBN  BVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce� Cg��Ci��Ck�3Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C�ٚC���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'l�D'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG��DHffDH�fDIffDI�fDJffDJ�fDKl�DK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy�fD��3D�#3D�l�D��fD�� D�,�D�fD���D��D�#3D�|�DǬ�D�� D� D�S3D�fD��fD�3D�)�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�(�A�%A���A���A���A�ƨA�ȴA���A��A��DA�dZA�I�A�A�A�1'A�+A�+A�-A�+A�(�A� �A��A�
=A���A��A��`A��#A���A���A���A�ĜA��FA���A���A��\A�=qA���A�XA��A�ffA�n�A��A�oA��A�ĜA�XA�E�A���A��A��7A��TA��A�JA�n�A���A��A�XA��A�
=A�G�A��;A���A���A�z�A���A���A�VA��\A��A�Q�A���A�n�A���A��A�oA��A�ĜA�{A��DA��TA�%A��PA�$�A�9XA�{A��A���A��;A�ƨA��A�/A�v�A�A~^5A{�AxbNAw+At�jAqoAp-An��Al�Ak7LAi��AgO�AeXAd�Ad-Ac�;Ac�7Ab�Aa%A_\)A]|�A\��A[AXȴAV{AU�AU�FASƨAR�uAQC�AO��AK�FAI�AG�AE�-AC��AAO�A?�A=ƨA<5?A:ĜA:Q�A;�A;K�A;7LA:��A9S�A8�RA8�+A7��A6�yA5/A3t�A3oA2��A2Q�A1��A0ȴA.Q�A,�A,I�A+�A'�^A&��A%�A%;dA$�!A$-A$�DA#�#A"Q�A!hsA ��A�AA��A�DA33AAdZA�yA��A��A�A�Ap�A��AM�A��A��A�#AoA�Ax�A{A"�A
�A
M�A	�7AE�A�AA;dA1A�yAp�A �jA E�@���@� �@�p�@�K�@���@�l�@��@�bN@�l�@�R@�J@�/@�K�@蛦@���@��@䛦@��T@���@ޗ�@��@�1@��@�z�@�dZ@֧�@��#@�?}@��`@Ԭ@�bN@��
@���@���@�I�@�l�@�ȴ@�7L@�o@ʗ�@�-@��@�1'@ǍP@�o@�n�@�x�@ļj@� �@�o@��-@��j@�|�@�5?@�?}@��@�|�@��\@�J@�G�@�A�@��@���@��@��^@���@��T@���@��P@��@��R@��H@���@��@�$�@���@��D@�E�@���@���@�(�@�33@�C�@��@��F@�;d@��@�;d@��@�
=@��@���@��+@�J@�?}@�Q�@��
@��P@���@�l�@�@��@���@�n�@��@�x�@��u@�ƨ@�t�@�
=@��T@�x�@�?}@���@�r�@��m@��F@�dZ@��@���@���@���@���@�v�@�^5@�E�@�=q@�5?@�-@��@�J@�@���@��@��-@���@���@���@��-@��^@��#@�$�@�E�@�^5@�n�@�-@�@���@�x�@���@�\)@�o@��u@��@���@��@��@���@�Q�@�  @��m@��m@���@��@�33@�"�@�
=@��H@���@�v�@��@��@��-@��@�O�@��@���@��@��u@�r�@�Z@�A�@� �@��m@��F@���@�\)@���@��@��!@�~�@�n�@�5?@��^@�G�@�%@��@���@�z�@�1'@��@���@�@���@���@��\@�v�@��@��T@��-@�hs@�G�@�V@���@��u@�z�@�9X@��@��@�l�@�+@���@�ȴ@��\@�V@�n�@��+@�5?@�J@��T@���@�G�@�?}@�7L@���@��u@�A�@�  @�ƨ@���@�\)@�o@�@��H@��@���@�v�@�=q@�@���@���@��@��@���@��-@�O�@���@�A�@� �@�1@��m@���@���@�l�@�S�@�33@�o@��@���@���@��+@�~�@�n�@�^5@�E�@�5?@�@���@���@w�w@pQ�@e��@\z�@V@M�h@FV@>V@8�`@1G�@+33@'�P@#��@�T@X@�/@bN@O�@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�(�A�%A���A���A���A�ƨA�ȴA���A��A��DA�dZA�I�A�A�A�1'A�+A�+A�-A�+A�(�A� �A��A�
=A���A��A��`A��#A���A���A���A�ĜA��FA���A���A��\A�=qA���A�XA��A�ffA�n�A��A�oA��A�ĜA�XA�E�A���A��A��7A��TA��A�JA�n�A���A��A�XA��A�
=A�G�A��;A���A���A�z�A���A���A�VA��\A��A�Q�A���A�n�A���A��A�oA��A�ĜA�{A��DA��TA�%A��PA�$�A�9XA�{A��A���A��;A�ƨA��A�/A�v�A�A~^5A{�AxbNAw+At�jAqoAp-An��Al�Ak7LAi��AgO�AeXAd�Ad-Ac�;Ac�7Ab�Aa%A_\)A]|�A\��A[AXȴAV{AU�AU�FASƨAR�uAQC�AO��AK�FAI�AG�AE�-AC��AAO�A?�A=ƨA<5?A:ĜA:Q�A;�A;K�A;7LA:��A9S�A8�RA8�+A7��A6�yA5/A3t�A3oA2��A2Q�A1��A0ȴA.Q�A,�A,I�A+�A'�^A&��A%�A%;dA$�!A$-A$�DA#�#A"Q�A!hsA ��A�AA��A�DA33AAdZA�yA��A��A�A�Ap�A��AM�A��A��A�#AoA�Ax�A{A"�A
�A
M�A	�7AE�A�AA;dA1A�yAp�A �jA E�@���@� �@�p�@�K�@���@�l�@��@�bN@�l�@�R@�J@�/@�K�@蛦@���@��@䛦@��T@���@ޗ�@��@�1@��@�z�@�dZ@֧�@��#@�?}@��`@Ԭ@�bN@��
@���@���@�I�@�l�@�ȴ@�7L@�o@ʗ�@�-@��@�1'@ǍP@�o@�n�@�x�@ļj@� �@�o@��-@��j@�|�@�5?@�?}@��@�|�@��\@�J@�G�@�A�@��@���@��@��^@���@��T@���@��P@��@��R@��H@���@��@�$�@���@��D@�E�@���@���@�(�@�33@�C�@��@��F@�;d@��@�;d@��@�
=@��@���@��+@�J@�?}@�Q�@��
@��P@���@�l�@�@��@���@�n�@��@�x�@��u@�ƨ@�t�@�
=@��T@�x�@�?}@���@�r�@��m@��F@�dZ@��@���@���@���@���@�v�@�^5@�E�@�=q@�5?@�-@��@�J@�@���@��@��-@���@���@���@��-@��^@��#@�$�@�E�@�^5@�n�@�-@�@���@�x�@���@�\)@�o@��u@��@���@��@��@���@�Q�@�  @��m@��m@���@��@�33@�"�@�
=@��H@���@�v�@��@��@��-@��@�O�@��@���@��@��u@�r�@�Z@�A�@� �@��m@��F@���@�\)@���@��@��!@�~�@�n�@�5?@��^@�G�@�%@��@���@�z�@�1'@��@���@�@���@���@��\@�v�@��@��T@��-@�hs@�G�@�V@���@��u@�z�@�9X@��@��@�l�@�+@���@�ȴ@��\@�V@�n�@��+@�5?@�J@��T@���@�G�@�?}@�7L@���@��u@�A�@�  @�ƨ@���@�\)@�o@�@��H@��@���@�v�@�=q@�@���@���@��@��@���@��-@�O�@���@�A�@� �@�1@��m@���@���@�l�@�S�@�33@�o@��@���@���@��+@�~�@�n�@�^5@�E�@�5?@�@���@���@w�w@pQ�@e��@\z�@V@M�h@FV@>V@8�`@1G�@+33@'�P@#��@�T@X@�/@bN@O�@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B'�BJ�Bp�B�B�?B�RB�wBÖBŢBŢBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�^B��B��B��B��B�JB|�Bt�BhsBdZB^5B]/B[#BW
BW
BW
BO�BK�BI�BA�B7LB0!B&�B�B�BDBPBB�B��B��BƨB�LB�'B��B��B��B�Br�B]/BS�BE�B8RB�BB
�sB
�B
ǮB
�FB
��B
�DB
p�B
ffB
\)B
N�B
H�B
=qB
"�B
bB
B	�B	�B	��B	��B	�qB	�3B	�LB	��B	�DB	�+B	�B	� B	� B	z�B	v�B	_;B	XB	XB	[#B	L�B	5?B	;dB	B�B	?}B	<jB	6FB	0!B	DB��B�fB�TB��BƨB�}B�^B�LB�3B�RB��B��B��B��B�}BƨB��B��BǮB�qB�'B�B�B�-B��B�dB��B�JB��B��Bt�Bp�Bm�Bs�Bv�B~�B�PB�VB�hB�PB�B~�B|�Bu�Bl�BcTBbNBe`BdZBcTBbNBbNB_;B[#BYBW
BT�BVBT�BS�BP�BN�BJ�BK�BL�BK�BH�BG�BL�BJ�BC�B;dB<jB:^B:^B:^B8RB<jB=qB?}BB�B49B7LB?}B8RB:^B8RB5?B2-B2-B2-B2-B;dBL�B9XB;dB9XB33B33B:^B;dB:^B:^B8RB7LB7LB8RB8RB7LB7LB;dB8RB7LB5?B7LB7LB8RB9XB<jB=qB@�B@�BE�BF�BI�BI�BN�BP�BP�BT�BW
B]/BaHBffBgmBn�Bo�Bo�Bt�Bt�Bv�B{�B�B�B�B�B�%B�7B�hB�hB��B��B��B�hB�+B�%B�JB�JB�DB�VB�\B�hB�PB�bB�uB��B��B��B��B�-B�?B�LB�XB�qBĜBǮB��B��B��B��B��B�B�B�#B�/B�`B�yB�sB�mB�mB�yB�B�B�B��B��B��B��B	B	+B	
=B	JB	PB	VB	\B	oB	uB	{B	�B	�B	�B	$�B	&�B	'�B	+B	-B	2-B	;dB	B�B	E�B	J�B	R�B	YB	\)B	aHB	\)B	XB	R�B	cTB	jB	m�B	o�B	p�B	q�B	s�B	v�B	w�B	w�B	w�B	w�B	y�B	|�B	~�B	� B	�B	�%B	�DB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�3B	�?B	�FB	�LB	�RB	�^B	�dB	�dB	�jB	�}B	�jB	�qB	�}B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
	7B
hB
�B
(�B
.B
6FB
<jB
C�B
H�B
O�B
T�B
XB
]/B
bNB
gmB
n�B
r�B
u�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B)�BM�Bu�B�B�?B�RB�}BĜBƨBƨBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�}B�B��B�B��B��B�+B}�Bn�BffB_;B_;B_;BYBZB[#BS�BO�BM�BH�B<jB33B(�B�B�BPBhB	7B��B��B��BɺB�XB�9B��B�B��B�=Bx�BaHBW
BI�BA�B"�B
=B
�B
�BB
��B
�wB
��B
�oB
v�B
jB
aHB
R�B
N�B
H�B
,B
�B
DB	��B	�/B	�B	�B	��B	�LB	�qB	��B	�PB	�1B	�B	�B	�B	� B	z�B	dZB	[#B	[#B	cTB	S�B	6FB	;dB	G�B	C�B	@�B	;dB	;dB	uB	B�B�B�B��BŢB��B�jB�?B�FB��B��B��B��B��BǮB��B��B��BÖB�3B�!B�B�9BŢBĜB��B�JB��B��Bw�Bs�Bp�Bu�Bx�B}�B�\B�oB�uB�\B�B�B� By�Bp�BgmBdZBgmBe`BffBe`Be`BaHB]/B[#BYBYBYBXBXBR�BS�BM�BL�BN�BN�BM�BL�BO�BM�BG�B?}BA�B=qB<jB>wB=qBA�BA�BC�BD�B6FB9XBA�B9XB;dB9XB8RB6FB5?B49B49B?}BO�B;dB=qB;dB6FB6FB<jB=qB<jB;dB9XB8RB8RB9XB:^B9XB:^B=qB:^B:^B9XB8RB8RB:^B;dB>wB>wBB�BB�BG�BH�BK�BL�BP�BS�BS�BW
BYB_;BcTBgmBiyBp�Bp�Bp�Bu�Bu�Bv�B{�B�B�B�B�B�%B�=B�oB�hB��B��B��B�uB�1B�+B�VB�JB�DB�VB�bB��B�bB�hB�uB��B��B��B��B�9B�LB�RB�^B�qBŢBȴB��B��B��B��B��B�B�#B�)B�5B�mB�B�yB�sB�sB�B�B�B�B��B��B��B��B	B	+B	
=B	JB	PB	VB	\B	oB	uB	{B	�B	�B	�B	$�B	&�B	'�B	+B	-B	2-B	;dB	B�B	E�B	K�B	S�B	ZB	]/B	bNB	_;B	YB	P�B	cTB	jB	n�B	o�B	p�B	r�B	t�B	v�B	w�B	w�B	x�B	x�B	y�B	|�B	� B	�B	�B	�+B	�JB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�-B	�9B	�FB	�FB	�RB	�XB	�dB	�jB	�jB	�qB	��B	�jB	�qB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�TB	�ZB	�`B	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
	7B
hB
�B
(�B
.B
6FB
=qB
C�B
H�B
O�B
T�B
YB
]/B
bNB
hsB
n�B
s�B
v�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452132012011014521320120110145213  AO  ARGQ                                                                        20111130144100  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144100  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145213  IP                  G�O�G�O�G�O�                