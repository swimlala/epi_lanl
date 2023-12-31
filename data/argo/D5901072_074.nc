CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:58Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               JA   AO  20111130144250  20190522121829  1728_5048_074                   2C  D   APEX                            2142                            040306                          846 @�⥸� 1   @��b�@@6{�l�C��b��+1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyy�D��D�&fD�i�D��3D�� D�)�D���D�� D���D�&fD�P Dǹ�D��fD�&fD�y�D๚D�� D� D�P D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33@�33A��A9��AY��Ay��A���A���A���A�  A�  A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�ffB�33B�33C��C��C��C��C	��C��C��C��C��C��C�3C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��D	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?� D@` D@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDH` DH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn��DoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDy` D�  D��D�\�D��fD��3D��D�|�D��3D�� D��D�C3DǬ�D��D��D�l�D��D��3D�3D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��TA��`A��`A��yA��yA��A��A���A�{A��A�+A�A�A�VA���A�5?A���A��A��A�5?A��\A��A�A�bA��A��mA�r�A�"�A���A���A��A��;A�K�A���A���A�\)A�\)A��mA�=qA�ȴA��DA���A�$�A��-A�1'A��+A�O�A��FA�1A��hA�K�A�/A�oA��#A�\)A�ȴA��\A�K�A��A���A�G�A�bA�&�A��A�I�A��mA��^A��A�`BA���A���A�M�A�-A��wA�;dA�ffA�&�A���A���A�t�A�1A�ƨA���A���A���A�r�A�;dA���A��A���A�bA�z�A��A��DA�ȴA��uA���A�n�A�A�\)A�x�A�
=A�
=A�jA�ffA�-A��A�/A��uA��A�\)A�(�A�(�A��PA��hA���A�bAp�A}O�Ay�mAw�Avv�Au\)Ar^5AqdZAp�Ap~�Ao�An�9Ak�#Af�RAd  Ab �A`jA_x�A^-A[��AY|�AU�wAQ��ANVAL  AH �AFZAD�ACt�AB~�A@�A?VA=K�A;�wA:�jA8ĜA7��A7��A7dZA7&�A6�!A5t�A2��A1�A0bNA/��A.�A-S�A,A*��A*�\A* �A)�#A(�A(^5A'�;A&�`A&A%�A%;dA$�yA$  A#/A"��A!�#A!%A ^5A�mA�A�uA�-A�RAA��AG�A�uAdZA�!AbA�wAx�AA�A�+A�AA�AVA��AVAJA�HA-AO�A�
A
��A	�AjA��A�9A�FA��A5?A�wA33A��A b@��@�A�@�dZ@���@�O�@�@��H@��@�X@�7L@��@�r�@�F@�
=@�@�D@�"�@�@�&�@�9X@�$�@䛦@�Z@��@�?}@�Ĝ@��m@���@��@ܛ�@�A�@�;d@��y@�V@�p�@�9X@�C�@��@ԛ�@Ӆ@ѡ�@��@ϕ�@��@�{@�X@�j@�  @�S�@�hs@��y@Ų-@�X@ă@�^5@�7L@�bN@���@��F@�1@�b@��
@��P@���@��@�G�@�S�@�ff@��#@��u@�|�@��+@�`B@�&�@���@�j@��;@�  @��/@��@�33@��y@�{@��@�K�@���@��!@��+@�=q@�-@�^5@�M�@�=q@��#@���@�`B@�A�@��@��H@���@��T@�/@��@�A�@���@�v�@���@��h@�p�@���@���@��@�$�@�O�@�(�@�
=@�M�@��@���@�X@��`@��9@���@�z�@�9X@���@���@�K�@���@���@�E�@�@�x�@�G�@�V@��/@�z�@�1'@��;@���@��w@�dZ@�
=@���@���@�^5@��@�@��^@��^@���@�@�?}@�I�@��w@��P@�dZ@�o@�@��H@���@�ff@�~�@�^5@��@���@��T@���@���@�%@���@� �@�1@��F@�33@�+@�l�@���@��@��!@�$�@�$�@���@��^@��h@�7L@��/@��j@��j@�Ĝ@��/@��j@��u@�bN@�9X@�1'@�  @��
@���@�|�@�+@���@�ȴ@�v�@�$�@��@���@��@�G�@���@���@���@��D@�j@�Q�@��@��@�ƨ@�t�@�33@�
=@��H@���@�M�@�-@�{@��@��T@���@��@�X@�?}@���@��j@��@�r�@�bN@�I�@��m@�ƨ@��@�dZ@�\)@�K�@�33@��@��@���@�~�@�ff@�^5@�M�@�J@�@���@��7@�p�@�`B@�7L@���@��@��D@�z�@�r�@�Q�@� �@��@��w@��P@��\@|�D@sdZ@j�\@a%@X �@S"�@N@F�R@AG�@<Z@8Q�@2�H@-��@&ȴ@�;@j@�;@9X@Q�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��TA��`A��`A��yA��yA��A��A���A�{A��A�+A�A�A�VA���A�5?A���A��A��A�5?A��\A��A�A�bA��A��mA�r�A�"�A���A���A��A��;A�K�A���A���A�\)A�\)A��mA�=qA�ȴA��DA���A�$�A��-A�1'A��+A�O�A��FA�1A��hA�K�A�/A�oA��#A�\)A�ȴA��\A�K�A��A���A�G�A�bA�&�A��A�I�A��mA��^A��A�`BA���A���A�M�A�-A��wA�;dA�ffA�&�A���A���A�t�A�1A�ƨA���A���A���A�r�A�;dA���A��A���A�bA�z�A��A��DA�ȴA��uA���A�n�A�A�\)A�x�A�
=A�
=A�jA�ffA�-A��A�/A��uA��A�\)A�(�A�(�A��PA��hA���A�bAp�A}O�Ay�mAw�Avv�Au\)Ar^5AqdZAp�Ap~�Ao�An�9Ak�#Af�RAd  Ab �A`jA_x�A^-A[��AY|�AU�wAQ��ANVAL  AH �AFZAD�ACt�AB~�A@�A?VA=K�A;�wA:�jA8ĜA7��A7��A7dZA7&�A6�!A5t�A2��A1�A0bNA/��A.�A-S�A,A*��A*�\A* �A)�#A(�A(^5A'�;A&�`A&A%�A%;dA$�yA$  A#/A"��A!�#A!%A ^5A�mA�A�uA�-A�RAA��AG�A�uAdZA�!AbA�wAx�AA�A�+A�AA�AVA��AVAJA�HA-AO�A�
A
��A	�AjA��A�9A�FA��A5?A�wA33A��A b@��@�A�@�dZ@���@�O�@�@��H@��@�X@�7L@��@�r�@�F@�
=@�@�D@�"�@�@�&�@�9X@�$�@䛦@�Z@��@�?}@�Ĝ@��m@���@��@ܛ�@�A�@�;d@��y@�V@�p�@�9X@�C�@��@ԛ�@Ӆ@ѡ�@��@ϕ�@��@�{@�X@�j@�  @�S�@�hs@��y@Ų-@�X@ă@�^5@�7L@�bN@���@��F@�1@�b@��
@��P@���@��@�G�@�S�@�ff@��#@��u@�|�@��+@�`B@�&�@���@�j@��;@�  @��/@��@�33@��y@�{@��@�K�@���@��!@��+@�=q@�-@�^5@�M�@�=q@��#@���@�`B@�A�@��@��H@���@��T@�/@��@�A�@���@�v�@���@��h@�p�@���@���@��@�$�@�O�@�(�@�
=@�M�@��@���@�X@��`@��9@���@�z�@�9X@���@���@�K�@���@���@�E�@�@�x�@�G�@�V@��/@�z�@�1'@��;@���@��w@�dZ@�
=@���@���@�^5@��@�@��^@��^@���@�@�?}@�I�@��w@��P@�dZ@�o@�@��H@���@�ff@�~�@�^5@��@���@��T@���@���@�%@���@� �@�1@��F@�33@�+@�l�@���@��@��!@�$�@�$�@���@��^@��h@�7L@��/@��j@��j@�Ĝ@��/@��j@��u@�bN@�9X@�1'@�  @��
@���@�|�@�+@���@�ȴ@�v�@�$�@��@���@��@�G�@���@���@���@��D@�j@�Q�@��@��@�ƨ@�t�@�33@�
=@��H@���@�M�@�-@�{@��@��T@���@��@�X@�?}@���@��j@��@�r�@�bN@�I�@��m@�ƨ@��@�dZ@�\)@�K�@�33@��@��@���@�~�@�ff@�^5@�M�@�J@�@���@��7@�p�@�`B@�7L@���@��@��D@�z�@�r�@�Q�@� �@��@��w@��P@��\@|�D@sdZ@j�\@a%@X �@S"�@N@F�R@AG�@<Z@8Q�@2�H@-��@&ȴ@�;@j@�;@9X@Q�@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�hB�hB�hB�bB�hB�bB�{B�{B��B�B��B�B�B��B��B��B	7BbB{B{B�B�B�B&�B'�B+B'�B#�B�B�B�B�B�B�B �B�BuB�B�B�B �B;dBC�B@�B<jB8RB6FB6FB49B49B49B49B49B49B6FB49B33B33B33B2-B6FB0!B-B1'B+B$�B!�B�B�BbBB  B��B��B�B�sB�mB�yB�yB�`B�TB�ZB�ZB�mB�B�B�mB�NB�;B�B��B�wB��B�7Bs�B]/B;dB�B{BB�B��B�jB�B��B~�B[#B>wB0!B'�B
�B
ɺB
�LB
��B
��B
x�B
[#B
?}B
-B
{B	��B	��B	�B	�TB	�`B	�fB	�`B	�BB	�#B	ŢB	��B	��B	�hB	�B	� B	x�B	l�B	cTB	G�B	+B	DB��B�fB�#B��B��B��B��B�qB�FB�!B�B��B��B��B��B��B��B�uB��B�oB�hB�PB�+B�B�\B�+B~�B}�B{�B{�By�Bv�Bt�Br�Bs�Bu�Br�Bo�Bo�Bn�Bm�Bl�Bl�Bl�Bk�BjBiyBiyBgmBffBffBffBe`BdZBe`BffBdZBcTBdZBbNB`BBaHB_;B\)B[#BZBYBXBXBVBW
BVBW
BT�BVBXBO�BP�BO�BK�BJ�B[#BJ�BA�BB�BC�BB�B:^B9XB:^B;dB@�BE�BH�BF�BD�BC�BF�BK�BF�BF�BH�BH�BG�BH�BJ�BK�BI�BG�BE�BD�BC�BC�BC�BB�BC�BD�BF�BH�BL�BM�BO�BQ�BO�BO�BR�BQ�BT�BYB\)B]/BcTBjBm�Bl�Br�B{�Bq�B|�B� B�B�JB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!BBǮBȴB��B�B��B��B�)B�BB�BB�ZB�B�B�B�B�B��B��B	B	B	B	DB	
=B	JB	PB	VB	oB	VB	VB	VB	\B	uB	bB	VB	oB	�B	{B	VB	PB	hB	JB	DB	VB	\B	�B	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	%�B	+B	)�B	+B	,B	-B	/B	1'B	5?B	8RB	;dB	>wB	@�B	B�B	C�B	F�B	H�B	M�B	VB	YB	[#B	\)B	_;B	`BB	aHB	dZB	gmB	l�B	l�B	q�B	x�B	{�B	}�B	� B	�B	�B	�+B	�1B	�=B	�DB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�-B	�9B	�LB	�RB	�^B	�jB	��B	��B	��B	B	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
DB
�B
$�B
(�B
/B
6FB
;dB
@�B
E�B
I�B
O�B
VB
\)B
cTB
ffB
k�B
n�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�hB�hB�hB�bB�hB�bB�{B��B��B�BB��B�B��B��BBVBoB�B�B�B �B%�B)�B,B.B)�B+B%�B�B�B�B�B�B#�B�B�B�B�B�B �B>wBF�BC�B@�B:^B:^B:^B7LB6FB5?B5?B6FB7LB9XB5?B5?B5?B5?B49B7LB49B/B33B-B%�B$�B �B�BoB+BB  B��B��B�yB�sB�B�B�mB�ZB�`B�ZB�mB�B�B�sB�TB�BB�)B��BÖB��B�PBy�BbNBA�B �B�B	7B�B��B��B�?B��B�7BcTBB�B49B8RB
��B
��B
�dB
�'B
��B
�B
dZB
F�B
8RB
�B	��B	��B	��B	�fB	�mB	�sB	�mB	�ZB	�TB	��B	�B	��B	��B	�%B	�B	}�B	r�B	l�B	Q�B	49B	oB	B�B�HB��B��B��B��BĜB�jB�9B�3B��B��B��B��B��B��B��B��B��B�{B�bB�DB�1B�oB�1B� B~�B~�B}�B{�By�Bw�Bt�Bt�Bv�Bu�Bq�Bq�Bp�Bo�Bn�Bn�Bo�Bm�Bm�Bl�Bk�BiyBhsBiyBjBhsBffBffBgmBffBdZBe`Be`Be`Be`BcTB_;B_;B^5B\)B[#B]/BYB[#B[#BZBXBYB[#BP�BQ�BP�BN�BN�B^5BL�BB�BE�BH�BE�B;dB;dB;dB<jBA�BF�BI�BG�BF�BF�BH�BM�BH�BH�BL�BK�BH�BJ�BM�BL�BK�BI�BH�BE�BD�BE�BD�BC�BE�BF�BH�BJ�BO�BO�BR�BT�BP�BQ�BT�BS�BW
BZB^5B`BBcTBl�Bn�Bm�Bu�B}�Br�B}�B� B�B�JB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BBȴB��B��B�B��B��B�/B�HB�BB�ZB�B�B�B�B��B��B��B	B	%B	B	JB	DB	PB	\B	\B	uB	\B	VB	\B	hB	�B	hB	\B	{B	�B	�B	\B	VB	oB	PB	JB	VB	bB	�B	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	%�B	&�B	,B	+B	+B	,B	.B	0!B	2-B	6FB	9XB	<jB	?}B	@�B	B�B	C�B	F�B	I�B	O�B	W
B	ZB	\)B	]/B	_;B	aHB	aHB	dZB	gmB	m�B	m�B	q�B	x�B	{�B	~�B	�B	�B	�B	�+B	�7B	�DB	�DB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�?B	�RB	�XB	�dB	�qB	��B	��B	��B	ÖB	ƨB	ƨB	ŢB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
DB
�B
$�B
(�B
/B
6FB
<jB
@�B
E�B
I�B
O�B
VB
\)B
cTB
gmB
k�B
o�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
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
<�o<T��<#�
<#�
<#�
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452192012011014521920120110145219  AO  ARGQ                                                                        20111130144250  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144250  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145219  IP                  G�O�G�O�G�O�                