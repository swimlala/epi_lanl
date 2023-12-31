CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:57Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               FA   AO  20111130144224  20190522121829  1728_5048_070                   2C  D   APEX                            2142                            040306                          846 @�ص�,��1   @�ض���@6�G�z��b�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3D y�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyL�D�fD�<�D���D�ɚD�3D�33D��fD�ɚD���D�fD�Y�D��3D���D�0 DچfD�3D��fD�fD�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.��B6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7�3C9�3C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�3Cg��Ci��Ck��Cm��Co��Cq�3Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C�� D ` D �fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
l�D
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)� D*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVl�DV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDy33D�	�D�0 D�� D���D��fD�&fD�y�D���D���D�	�D�L�DǶfD���D�#3D�y�D�fD�ٚD���D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�M�A�S�A�Q�A�C�A�A�A�33A�{A�
=A�%A�A�  A�  A�  A���A���A���A���A���A���A���A���A���A���A��A��A��A�ȴA�5?A��A�I�A�"�A�jA��A�~�A��A��A�-A�{A��mA��^A��uA��A���A��7A�(�A��hA�XA�ffA��+A��uA�|�A�I�A��mA�K�A��;A�{A��^A���A�t�A�oA��HA���A��\A�(�A���A�n�A�C�A��A���A���A�~�A��A�ȴA�l�A�A��A�x�A���A�r�A��`A�Q�A��
A�G�A���A�/A�?}A��#A���A��RA��HA���A��HA��!A�(�A���A�$�A���A�jA��jA��A�M�A�VA�&�A�bNA��A�O�A���A�C�A�E�A��A���A��wA���A�t�A��A�ZA�5?A���A�`BA���A�Q�A��\A~JAy?}Aw
=AshsAp�`AkAh��Ad��Ac�wAc
=Aa�^AaK�A`�+A]t�AY�AX�\AV�AR�jAO�TANE�AJ��AI`BAH�!AG�AEC�AC+AA�A@ĜA>��A<��A:�`A9?}A7?}A6 �A5��A5A4�A4 �A2�uA0��A/C�A-��A,bA+A)�#A(~�A'A%�hA$�A#�TA#;dA"r�A!A �A r�A (�A�wA��A �A��A�AO�A��A=qA��AA�AO�AVA^5Ap�A�!A�#A�7A+A��AbA\)A�jA��A  A5?A��At�A%A
�RA
bNA	�^A�A��A��A��AK�A?}A�RA�TA33Az�A�A��AXA r�@��@�M�@���@�r�@��@�E�@���@��P@���@��@��m@�ƨ@���@�^@�Ĝ@�@��@�V@�bN@�@��@�5?@�r�@�=q@�b@�v�@�7L@�bN@ۮ@�M�@�G�@��;@�o@�-@ԣ�@�\)@�o@�"�@�33@�o@�5?@�x�@�Z@�S�@��T@�X@��@�1'@ʰ!@�E�@�@ȃ@��#@�bN@�1@��
@�\)@�33@���@�E�@��@�p�@���@�I�@�ȴ@���@�hs@���@��F@��@�dZ@�;d@�dZ@�j@�G�@��/@�Z@�1@��!@�hs@�|�@�M�@���@��@�/@�/@�hs@��9@���@���@�V@�7L@���@��@���@��j@���@�A�@�ƨ@�"�@�@���@��T@��@�r�@��#@��@���@��y@��#@���@� �@��P@�+@���@�M�@���@��@���@�/@���@�V@�bN@��@��F@���@�|�@�K�@�"�@��@�ȴ@�E�@��T@�@���@�x�@�/@��@��@�9X@��F@�C�@�~�@�{@���@�7L@�/@��/@��j@���@��D@� �@��;@���@�\)@�o@��@�+@�
=@��H@��@��H@��@�t�@�\)@�@��@�M�@�5?@�5?@�=q@�ff@��+@�v�@�J@���@��9@�j@�bN@�bN@�Z@��@��;@��F@��@��@��@���@�E�@��@�@�x�@�G�@��`@���@�z�@� �@��w@���@���@�\)@�K�@�o@���@�v�@�{@��#@���@���@�@��h@�p�@�X@�?}@�/@��@���@��/@���@���@���@���@�Q�@�1'@�(�@�(�@�1'@�1'@�(�@� �@� �@�1@�ƨ@�"�@��@��@��y@���@��H@�v�@�{@��@���@�7L@��@��@���@��@��@�z�@�j@�I�@�1@��m@��F@��@�;d@�
=@��@��R@��+@�v�@�^5@�J@��-@��7@�X@�&�@���@�Z@x  @rJ@l�D@d�@\��@U�@Pr�@I��@C��@;�F@3��@0 �@*��@$I�@��@�\@5?@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�M�A�S�A�Q�A�C�A�A�A�33A�{A�
=A�%A�A�  A�  A�  A���A���A���A���A���A���A���A���A���A���A��A��A��A�ȴA�5?A��A�I�A�"�A�jA��A�~�A��A��A�-A�{A��mA��^A��uA��A���A��7A�(�A��hA�XA�ffA��+A��uA�|�A�I�A��mA�K�A��;A�{A��^A���A�t�A�oA��HA���A��\A�(�A���A�n�A�C�A��A���A���A�~�A��A�ȴA�l�A�A��A�x�A���A�r�A��`A�Q�A��
A�G�A���A�/A�?}A��#A���A��RA��HA���A��HA��!A�(�A���A�$�A���A�jA��jA��A�M�A�VA�&�A�bNA��A�O�A���A�C�A�E�A��A���A��wA���A�t�A��A�ZA�5?A���A�`BA���A�Q�A��\A~JAy?}Aw
=AshsAp�`AkAh��Ad��Ac�wAc
=Aa�^AaK�A`�+A]t�AY�AX�\AV�AR�jAO�TANE�AJ��AI`BAH�!AG�AEC�AC+AA�A@ĜA>��A<��A:�`A9?}A7?}A6 �A5��A5A4�A4 �A2�uA0��A/C�A-��A,bA+A)�#A(~�A'A%�hA$�A#�TA#;dA"r�A!A �A r�A (�A�wA��A �A��A�AO�A��A=qA��AA�AO�AVA^5Ap�A�!A�#A�7A+A��AbA\)A�jA��A  A5?A��At�A%A
�RA
bNA	�^A�A��A��A��AK�A?}A�RA�TA33Az�A�A��AXA r�@��@�M�@���@�r�@��@�E�@���@��P@���@��@��m@�ƨ@���@�^@�Ĝ@�@��@�V@�bN@�@��@�5?@�r�@�=q@�b@�v�@�7L@�bN@ۮ@�M�@�G�@��;@�o@�-@ԣ�@�\)@�o@�"�@�33@�o@�5?@�x�@�Z@�S�@��T@�X@��@�1'@ʰ!@�E�@�@ȃ@��#@�bN@�1@��
@�\)@�33@���@�E�@��@�p�@���@�I�@�ȴ@���@�hs@���@��F@��@�dZ@�;d@�dZ@�j@�G�@��/@�Z@�1@��!@�hs@�|�@�M�@���@��@�/@�/@�hs@��9@���@���@�V@�7L@���@��@���@��j@���@�A�@�ƨ@�"�@�@���@��T@��@�r�@��#@��@���@��y@��#@���@� �@��P@�+@���@�M�@���@��@���@�/@���@�V@�bN@��@��F@���@�|�@�K�@�"�@��@�ȴ@�E�@��T@�@���@�x�@�/@��@��@�9X@��F@�C�@�~�@�{@���@�7L@�/@��/@��j@���@��D@� �@��;@���@�\)@�o@��@�+@�
=@��H@��@��H@��@�t�@�\)@�@��@�M�@�5?@�5?@�=q@�ff@��+@�v�@�J@���@��9@�j@�bN@�bN@�Z@��@��;@��F@��@��@��@���@�E�@��@�@�x�@�G�@��`@���@�z�@� �@��w@���@���@�\)@�K�@�o@���@�v�@�{@��#@���@���@�@��h@�p�@�X@�?}@�/@��@���@��/@���@���@���@���@�Q�@�1'@�(�@�(�@�1'@�1'@�(�@� �@� �@�1@�ƨ@�"�@��@��@��y@���@��H@�v�@�{@��@���@�7L@��@��@���@��@��@�z�@�j@�I�@�1@��m@��F@��@�;d@�
=@��@��R@��+@�v�@�^5@�J@��-@��7@�X@�&�@���@�Z@x  @rJ@l�D@d�@\��@U�@Pr�@I��@C��@;�F@3��@0 �@*��@$I�@��@�\@5?@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB+B%B%B+B+B	7B	7B
=B
=BDBDBDB
=BDBDBDBDBDBDBDBDBDBDBDBDBDBDBhB%�B>wBL�BG�BG�BaHBm�B|�B�VB��B��B��B��B�B��B��BÖB�3B�B�3BB��B�B�NB�B��BǮB�jB�qB��B�/B�5B�#B�;B�B�TB�;B�5B�#B�B�5B�BB�NB�5B�/B�B�B�B�#B��BǮB�}B�qB�9B�B��Bs�BR�BD�BXBdZBYB'�BB��B�`B�BÖB�wB�B��B��Bv�BgmBVBH�B=qB33B)�BoBB
��B
��B
�B
�HB
��B
ɺB
��B
�LB
�B
�B
`BB
E�B
<jB
�B	�B	�BB	��B	��B	�=B	s�B	P�B	M�B	G�B	?}B	:^B	1'B	�B	uB		7B��B�ZB��B��B�qB�?B�'B��B��B��B�uB�\B�=B� B{�B}�Bv�Bt�Bt�Bv�Br�Bo�Bn�Bt�B{�Bu�Bm�Bt�Bu�BiyBiyBiyBp�Bo�Bl�Bk�Bo�Bp�Bl�BjBiyBm�BjBffBdZBcTBbNBbNBaHBbNB`BB_;B_;B_;B^5B^5B_;B^5B^5B_;B^5B`BB[#B[#B[#B[#B[#B[#BZBXBW
BXBVBT�BS�BS�BR�BR�BR�BR�BVBS�B[#BQ�BZB[#B[#BW
B[#BYBVB^5BW
BYBXBYBXBW
BXB^5BZBS�BR�BQ�BP�BO�BM�BP�BQ�BQ�BI�BO�BJ�BI�BI�BF�BK�BM�BO�BO�BL�BN�BW
B\)B`BBbNBcTBcTBe`Be`Bm�Bw�B{�B{�B~�B�+B�1B��B�hB�bB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�}B��B��B��B��B��B�B��B��B��B��B�B�#B�HB�TB�B��B��B��B��B��B��B	  B	B	  B��B��B��B�B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	1B	
=B	JB	VB	hB	uB	{B	�B	�B	�B	!�B	!�B	"�B	$�B	%�B	&�B	+B	,B	.B	0!B	1'B	1'B	1'B	33B	8RB	9XB	9XB	:^B	<jB	@�B	B�B	D�B	I�B	N�B	R�B	T�B	XB	ZB	]/B	_;B	bNB	jB	m�B	o�B	t�B	u�B	u�B	v�B	z�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�FB	�RB	�RB	�RB	�dB	�qB	�jB	�}B	��B	B	ĜB	ÖB	B	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�)B	�/B	�/B	�/B	�5B	�BB	�BB	�BB	�HB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	��B
B
\B
�B
�B
'�B
.B
33B
9XB
>wB
F�B
N�B
Q�B
YB
^5B
e`B
hsB
m�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB+B%B%B+B+B
=B	7B
=B
=BDBDBDB
=BDBDBDBDBDBDBDBDBDBDBDBDBJBPB�B-BC�BT�BN�BI�BcTBm�B|�B�\B��B��B��B��B�B��B��BǮB�9B�B�-BB��B�B�ZB�/B��B��B�wB�wB��B�;B�;B�)B�BB�B�fB�BB�;B�)B�)B�NB�TB�ZB�BB�;B�B�#B�5B�BB��B��BB��B�LB�!B�B~�B[#BE�BXBiyBdZB1'B+BB�B�BBȴB��B�B��B��B}�Bl�BZBL�B?}B6FB49B�BB
��B
��B
��B
�mB
�B
��B
ǮB
�}B
�^B
�JB
ffB
H�B
A�B
 �B	��B	�sB	��B	�3B	�hB	~�B	S�B	O�B	K�B	@�B	=qB	9XB	&�B	�B	bB��B�B�B��BB�LB�FB�B��B��B��B��B�bB�%B�B�Bz�Bv�Bv�Bw�Bt�Bs�Br�Bx�B� By�Bn�By�By�Bm�Bm�Bl�Br�Bq�Bo�Bm�Br�Br�Bm�Bl�Bl�Bp�Bn�BiyBffBe`BdZBdZBffBe`BaHBbNBbNBbNBaHB_;BaHB`BBaHBaHB`BBdZB`BBaHB]/B]/B]/B\)B\)BZBZB]/BZBYBT�BT�BT�BVBT�BT�BW
BT�B\)BT�B\)B]/B\)BYB]/B[#BXB`BBZB[#BZBZBZBYBZBcTB_;BW
BT�BS�BR�BQ�BP�BT�BVBT�BK�BQ�BL�BL�BK�BI�BK�BO�BO�BQ�BM�BN�BW
B]/BbNBdZBe`Be`BhsBffBn�By�B~�B|�B� B�7B�1B��B�oB�hB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�wB��B��B��B�B�B�)B��B��B��B��B�B�#B�NB�TB�B��B��B��B��B��B��B	B	B	B��B	B��B��B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B		7B	DB	PB	VB	oB	{B	�B	�B	�B	 �B	!�B	!�B	#�B	%�B	&�B	'�B	,B	-B	/B	1'B	2-B	2-B	2-B	33B	9XB	9XB	9XB	:^B	=qB	A�B	C�B	E�B	J�B	N�B	R�B	VB	YB	ZB	]/B	_;B	bNB	jB	n�B	p�B	u�B	u�B	u�B	v�B	z�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�LB	�XB	�RB	�RB	�jB	�qB	�qB	��B	��B	ÖB	ŢB	ÖB	B	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B
B
\B
�B
�B
'�B
.B
33B
9XB
>wB
F�B
N�B
R�B
YB
^5B
e`B
iyB
m�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452172012011014521720120110145217  AO  ARGQ                                                                        20111130144224  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144224  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145217  IP                  G�O�G�O�G�O�                