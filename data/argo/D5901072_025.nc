CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:46Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143733  20190522121828  1728_5048_025                   2C  D   APEX                            2142                            040306                          846 @�h�Al@	1   @�h��s�@5��hr��c6� ě�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy� D���D�,�D�|�D���D�� D��D��3D���D��fD�  D��3Dǳ3D��D�  D�VfD�� D�� D�fD�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@�33@�33A��A9��AY��Ay��A���A���A���A���A���A�  A�  A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�ffB�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3� C5��C7��C9��C;��C=��C?��CA�3CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�3C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fD` D�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<` D<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDG` DG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL��DMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnl�Dn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDw` Dy�fD���D�  D�p D�� D��3D��D�vfD���D�ٚD�3D�vfDǦfD���D�3D�I�D��3D��3D���D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�S�A�\)A�ZA�^5A�M�A�5?A�9XA�+A��A��A�A��uA��wA��A�+A���A��A�bNA�?}A�+A��A���A��7A��A�ƨA���A��A�x�A�E�A��A�33A�A�l�A�oA��!A�XA�v�A�bA���A�G�A� �A��/A�I�A�
=A�1A��#A���A���A�I�A�VA���A��A�A���A�dZA��HA�S�A�v�A�"�A��jA�\)A�7LA�A��A��mA�A��
A��yA�r�A���A��!A��yA��A�5?A���A�n�A�hsA�S�A��`A��FA�/A��A��7A��mA�-A�Q�A��PA��A�|�A�ƨA�A�Q�A�%A��!A��+A�XA��jA�7LA�A��A��wA��!A�1A�Q�A��/A��/A��A~ffA{��AyG�Av��Ar$�ApE�AnVAl��Af��Ab1'A`ZA_VA]�A\�A[�FAZ�DAY�TAY"�AW�AWC�AU�^AUC�ATbNAR�!AQ��AQ�AP��AP1'AOƨAO`BAO33AO�AN�RANQ�AMG�AKC�AJAI�AH$�AGl�AE�AC�A@JA<M�A:z�A9VA7�FA6�/A6�A6��A5�TA5�7A5"�A4r�A3K�A1��A0��A0 �A/dZA.JA,�jA,I�A,{A+XA*z�A)�A'�mA&��A$M�A"�jA!��A!?}A �AA�`A��AVAJA�mA�7AVAE�A�A�7A�`Az�A�;A�AffA��A�A/A��A��A�A �A��A��A�^A	�PAE�A�PAȴA�^AbA~�A��AdZA n�A @��#@��m@�V@�9X@�E�@��u@���@�V@�@�$�@��;@旍@�p�@�9X@�n�@�33@��@�X@�-@��m@�r�@�9X@���@��m@���@�ƨ@ۮ@�t�@ڗ�@ّh@ش9@�v�@Ցh@�@�$�@��@�hs@�?}@�%@ԓu@�I�@�1'@�b@��
@�
=@���@�Ĝ@�"�@�@ͩ�@�`B@��@��@�&�@��@�ƨ@��T@˾w@���@ɡ�@��@�\)@�E�@ģ�@�K�@��#@��@�O�@��#@���@��h@�X@��@��9@��@��w@�t�@�C�@�
=@�~�@�M�@�-@�J@�X@��`@�A�@��@���@��P@��R@�E�@���@��^@��@��@�z�@��@�l�@�33@�\)@���@�=q@�@�O�@���@���@��D@� �@���@��@�C�@��@�+@��@���@���@��+@�-@��@���@�&�@��/@�I�@�1@��@��@�v�@��u@�1'@���@�t�@�dZ@�33@�ȴ@��@���@�J@��@�-@�5?@�{@��-@��7@�X@�7L@��@���@���@�I�@�b@��@�"�@��R@�n�@�E�@�$�@��@���@�X@��@���@��D@�Z@� �@��@�  @���@��@��m@���@�dZ@���@�n�@�M�@�5?@�@���@��@�7L@�/@�/@�7L@�&�@��@��j@��D@�I�@�1@��m@���@��@���@�{@���@��@�?}@���@�Ĝ@��j@��j@��u@���@��F@�l�@��@���@�$�@���@���@��^@���@�x�@�G�@���@�  @�K�@��H@�v�@�5?@�$�@�J@���@���@��7@�X@�&�@�V@�V@�%@���@���@��9@���@�bN@�A�@�(�@��@��m@�t�@���@�J@���@��@�X@�G�@�/@�&�@��@��@��@���@�9X@��m@��F@�|�@�o@�@�@���@��@��R@�v�@���@���@��@�`B@�/@��9@�A�@�b@�  @��@��
@��@�t�@�;d@�E�@�&�@~ȴ@s��@kdZ@`��@\��@S�m@K"�@A�@<�/@7l�@2�H@+ƨ@$9X@$�@�@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�O�A�S�A�\)A�ZA�^5A�M�A�5?A�9XA�+A��A��A�A��uA��wA��A�+A���A��A�bNA�?}A�+A��A���A��7A��A�ƨA���A��A�x�A�E�A��A�33A�A�l�A�oA��!A�XA�v�A�bA���A�G�A� �A��/A�I�A�
=A�1A��#A���A���A�I�A�VA���A��A�A���A�dZA��HA�S�A�v�A�"�A��jA�\)A�7LA�A��A��mA�A��
A��yA�r�A���A��!A��yA��A�5?A���A�n�A�hsA�S�A��`A��FA�/A��A��7A��mA�-A�Q�A��PA��A�|�A�ƨA�A�Q�A�%A��!A��+A�XA��jA�7LA�A��A��wA��!A�1A�Q�A��/A��/A��A~ffA{��AyG�Av��Ar$�ApE�AnVAl��Af��Ab1'A`ZA_VA]�A\�A[�FAZ�DAY�TAY"�AW�AWC�AU�^AUC�ATbNAR�!AQ��AQ�AP��AP1'AOƨAO`BAO33AO�AN�RANQ�AMG�AKC�AJAI�AH$�AGl�AE�AC�A@JA<M�A:z�A9VA7�FA6�/A6�A6��A5�TA5�7A5"�A4r�A3K�A1��A0��A0 �A/dZA.JA,�jA,I�A,{A+XA*z�A)�A'�mA&��A$M�A"�jA!��A!?}A �AA�`A��AVAJA�mA�7AVAE�A�A�7A�`Az�A�;A�AffA��A�A/A��A��A�A �A��A��A�^A	�PAE�A�PAȴA�^AbA~�A��AdZA n�A @��#@��m@�V@�9X@�E�@��u@���@�V@�@�$�@��;@旍@�p�@�9X@�n�@�33@��@�X@�-@��m@�r�@�9X@���@��m@���@�ƨ@ۮ@�t�@ڗ�@ّh@ش9@�v�@Ցh@�@�$�@��@�hs@�?}@�%@ԓu@�I�@�1'@�b@��
@�
=@���@�Ĝ@�"�@�@ͩ�@�`B@��@��@�&�@��@�ƨ@��T@˾w@���@ɡ�@��@�\)@�E�@ģ�@�K�@��#@��@�O�@��#@���@��h@�X@��@��9@��@��w@�t�@�C�@�
=@�~�@�M�@�-@�J@�X@��`@�A�@��@���@��P@��R@�E�@���@��^@��@��@�z�@��@�l�@�33@�\)@���@�=q@�@�O�@���@���@��D@� �@���@��@�C�@��@�+@��@���@���@��+@�-@��@���@�&�@��/@�I�@�1@��@��@�v�@��u@�1'@���@�t�@�dZ@�33@�ȴ@��@���@�J@��@�-@�5?@�{@��-@��7@�X@�7L@��@���@���@�I�@�b@��@�"�@��R@�n�@�E�@�$�@��@���@�X@��@���@��D@�Z@� �@��@�  @���@��@��m@���@�dZ@���@�n�@�M�@�5?@�@���@��@�7L@�/@�/@�7L@�&�@��@��j@��D@�I�@�1@��m@���@��@���@�{@���@��@�?}@���@�Ĝ@��j@��j@��u@���@��F@�l�@��@���@�$�@���@���@��^@���@�x�@�G�@���@�  @�K�@��H@�v�@�5?@�$�@�J@���@���@��7@�X@�&�@�V@�V@�%@���@���@��9@���@�bN@�A�@�(�@��@��m@�t�@���@�J@���@��@�X@�G�@�/@�&�@��@��@��@���@�9X@��m@��F@�|�@�o@�@�@���@��@��R@�v�@���@���@��@�`B@�/@��9@�A�@�b@�  @��@��
@��@�t�@�;d@�E�@�&�@~ȴ@s��@kdZ@`��@\��@S�m@K"�@A�@<�/@7l�@2�H@+ƨ@$9X@$�@�@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�#B
�#B
�B
�)B
�B
�B
�B
�#B
�)B
�)B
�5B
�ZB�BiyB�B�TB��B�B7LBC�BI�BQ�BXBiyBq�Bq�Bx�B�+B��B�B�B��B��B�FBŢB��B�BǮB�qB�qB�jB�jB�jB�^B�^B�^B�dB�dB�wB�LB�FB�9B�9B�B��B��B��B��B��B��B��B�uB�bB�=By�Bn�BgmBdZBT�BM�BA�B6FB.B#�B�BhBDB
=B+BB�B�B�TB��BB�XB��B��B�JBl�BQ�BC�B8RB(�B!�B�B�BhBB
��B
�}B
�7B
|�B
p�B
bNB
[#B
J�B
<jB
.B
{B
B	�B	��B	��B	��B	�3B	�JB	z�B	t�B	p�B	e`B	^5B	XB	O�B	I�B	D�B	?}B	>wB	B�B	@�B	=qB	2-B	.B	,B	)�B	,B	,B	-B	-B	,B	/B	0!B	.B	.B	.B	+B	(�B	"�B	�B	B�B��BǮB�qB�?B�?B�wB��BǮBŢBB�}B�LB�'B�-B�B��B�B��B��B��B��B��B��B��B��B�uB�\B�PB�\B�PB�DB�1B�+B�B�B�B�B�B�B}�B{�B{�B}�By�B}�B�Bw�Bz�Bq�Br�Bu�Bu�Bv�Bw�Bx�Bz�B|�By�Bx�Bz�B�+B�VBw�Bt�Bs�Bp�Bl�Bt�Bq�Bp�Bl�Bp�Bt�Bn�BffBffBgmB\)BT�BO�BN�BI�BC�B@�BD�BO�BaHBq�B~�B�B�%B�=B�VB�uB��B��B��B��B��B��B��B��B�B�3B�3B�9B�FB�RB�XB�^B�dB�qB�}B��BǮB��B��B��B��B��B��B�B�
B��B��B	\B	JB	hB	�B	!�B	 �B	"�B	"�B	$�B	33B	<jB	C�B	D�B	E�B	E�B	F�B	I�B	J�B	L�B	N�B	O�B	T�B	W
B	W
B	YB	]/B	`BB	cTB	e`B	ffB	k�B	p�B	q�B	t�B	u�B	v�B	v�B	v�B	{�B	u�B	t�B	w�B	}�B	� B	�B	�B	�B	�B	�%B	�%B	�=B	�VB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�?B	�FB	�FB	�FB	�FB	�RB	�XB	�XB	�^B	�jB	�wB	�wB	�}B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�
B	�B	�#B	�B	�)B	�/B	�)B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
hB
�B
"�B
-B
2-B
;dB
D�B
H�B
L�B
Q�B
VB
\)B
dZB
iyB
o�B
r�B
v�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�#B
�#B
�B
�)B
�B
�B
�B
�#B
�)B
�)B
�5B
�`B�Bk�B�B�ZB��B�B8RBD�BJ�BR�BZBk�Bs�Br�By�B�1B��B�-B�-B�B��B�RBǮB��B�#BɺB�}B�}B�qB�qB�wB�dB�^B�dB�wB��BĜB�dB�RB�FB�FB�-B�B��B��B��B��B��B��B��B��B�{B~�Br�Bl�BhsBW
BQ�BE�B:^B49B%�B�BoBDBDB	7BDB�B�B�yB��BƨB�wB�B��B��Bv�BW
BH�B@�B+B"�B �B�B�BVB\B
��B
�bB
�B
t�B
e`B
aHB
N�B
@�B
2-B
�B
1B	�B	�B	��B	��B	��B	��B	� B	x�B	s�B	hsB	bNB	[#B	Q�B	L�B	H�B	B�B	C�B	D�B	C�B	C�B	5?B	/B	.B	-B	.B	.B	.B	.B	.B	1'B	49B	5?B	2-B	2-B	/B	,B	(�B	!�B	PB	  B�
B��BB�RB�?B�}B��BɺBǮBŢBĜB�jB�?B�?B�B�B�!B��B��B��B��B��B��B��B��B��B�hB�\B�hB�\B�VB�JB�DB�%B�%B�%B�%B�%B�1B�B~�B}�B� B|�B�B�B|�B~�Bv�Bv�Bv�Bw�Bx�Bz�B}�B�B�B|�B{�B~�B�JB�oBy�Bu�Bu�Bq�Bo�Bw�Bs�Bs�Bo�Bs�Bz�Bq�BiyBjBk�B^5BW
BQ�BQ�BN�BJ�BC�BC�BM�BaHBr�B� B�B�%B�DB�\B�{B��B��B��B��B��B��B��B��B�B�9B�9B�?B�LB�XB�^B�dB�qB�}BBĜBɺB��B��B��B��B��B��B�#B�B��B��B	hB	VB	oB	�B	$�B	"�B	%�B	$�B	#�B	2-B	<jB	D�B	E�B	F�B	F�B	G�B	J�B	K�B	M�B	O�B	P�B	VB	XB	XB	[#B	^5B	aHB	dZB	ffB	gmB	k�B	q�B	r�B	t�B	w�B	w�B	w�B	w�B	|�B	v�B	t�B	x�B	~�B	�B	�B	�B	�B	�%B	�+B	�+B	�=B	�\B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�FB	�LB	�FB	�LB	�LB	�XB	�^B	�^B	�dB	�qB	�}B	�}B	��B	��B	B	B	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�
B	�
B	�
B	�B	�)B	�#B	�/B	�5B	�/B	�;B	�;B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�TB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
hB
�B
#�B
-B
2-B
;dB
E�B
H�B
L�B
Q�B
VB
]/B
dZB
iyB
o�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<���<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452012012011014520120120110145201  AO  ARGQ                                                                        20111130143733  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143733  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145201  IP                  G�O�G�O�G�O�                