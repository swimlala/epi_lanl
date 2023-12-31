CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:02:00Z UW 3.1 conversion   
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
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               NA   AO  20111130144316  20190522121829  1728_5048_078                   2C  D   APEX                            2142                            040306                          846 @�앨�1   @��5��@6�hr��b׍O�;d1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B ffB  B��B��B   B(  B0  B8ffB@  BG��BP  BX  B`  Bg��Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�33C   C�fC  C�C  C
  C  C�fC  C  C�fC  C  C  C�C  C�fC"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C7�fC:  C<�C>  C@  CB  CC�fCF  CH  CJ  CL  CN�CP  CR  CT  CV�CX�CZ  C[�fC]�fC`  Cb  Cd  Cf  Cg�fCj  Cl�Cn  Cp  Cq�fCt  Cv�Cx  Cz  C|�C~  C�fC�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C��C��C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  D   D � DfD� D  D� D  D� D��D� D  D� DfD�fD  D� D  D� D	  D	�fD
fD
�fDfD�fDfD�fDfD�fDfD� D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&�fD'fD'�fD(fD(�fD)fD)�fD*  D*� D+  D+y�D+��D,� D-fD-�fD.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D5��D6� D7  D7� D8fD8� D9  D9� D:  D:�fD;  D;y�D<  D<� D=  D=y�D>  D>�fD?fD?� D@  D@� DA  DAy�DA��DB� DC  DCy�DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dy��D�fD�33D���D��fD�  D�)�D�33D��fD�� D�,�D�l�D��fD��D� D�l�D�3D�� D��D�L�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@   @fff@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffB  B  BffB&ffB.ffB6��B>ffBF  BNffBVffB^ffBf  BnffBv��B~��B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�  B�33B�ffB�33B�33B�33B�ffB�33B�33B�33B�33B�33B�ffB�ffB�33B�  B�33B�ffB�33C� C��C�3C��C	��C��C� C��C��C� C��C��C��C�3C��C� C!��C#��C%��C'��C)��C+�3C-��C/��C1��C3��C5��C7� C9��C;�3C=��C?��CA��CC� CE��CG��CI��CK��CM�3CO��CQ��CS��CU�3CW�3CY��C[� C]� C_��Ca��Cc��Ce��Cg� Ci��Ck�3Cm��Co��Cq� Cs��Cu�3Cw��Cy��C{�3C}��C� C���C���C�� C���C���C�ٚC�ٚC���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C�ٚC���C���C�ٚC���C�� C�� C�� C�� C�� C���C���C���C�� C�� C���C�ٚC���C���C�� C���C�ٚC���C���C���C���C�� C���C�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�� C�� C�� C�� C�� C���C�ٚC���C�� C���C���C�� C���C�ٚC�ٚC���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�ٚC���C���C���C���C���C���C�� C�� C���C���C�� C�� C�� C�� C���C�ٚC�ٚC�ٚC���C���C���C���C�� C���C���C�� C���C���C�� C���C���C�� C���C���C�� C���C���C���C���D ffD ��DffD�fDffD�fDffD� DffD�fDffD��Dl�D�fDffD�fDffD�fD	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��DffD� D` D� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"� D#ffD#�fD$ffD$�fD%ffD%�fD&l�D&��D'l�D'��D(l�D(��D)l�D)�fD*ffD*�fD+` D+� D,ffD,��D-l�D-��D.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4` D4�fD5ffD5� D6ffD6�fD7ffD7��D8ffD8�fD9ffD9�fD:l�D:�fD;` D;�fD<ffD<�fD=` D=�fD>l�D>��D?ffD?�fD@ffD@�fDA` DA� DBffDB�fDC` DC�fDDffDD�fDEl�DE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPl�DP�fDQ` DQ� DRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[` D[�fD\ffD\�fD]ffD]�fD^ffD^�fD_l�D_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDl` Dl� DmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvl�Dv�fDwffDy� D���D�&fD�|�D���D��3D��D�&fD���D��3D�  D�` Dǹ�D���D�3D�` D��fD��3D��D�@ D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˗�AˋDAˉ7AˍPA˙�A˝�A˙�A˗�A˟�Aˡ�A˅A�z�A�z�A�;dA�"�A�&�A�/A�K�A�E�A�?}A�7LA�-A�"�A� �A�{A��Aʟ�A�A�A�;dA�7LA�33A�/A� �A�A��Aɩ�AȰ!A�5?A�Q�A���A�A��DA���A�bA�|�A��!A�JA�ffA�?}A���A�G�A�+A��A�"�A��-A��A�dZA�A��hA�$�A��A�ĜA���A�ĜA��\A��A�A��DA��A�(�A��\A��-A���A���A�VA�bNA���A�I�A���A�O�A�%A��A�-A�VA��A��A�&�A��jA�1'A�ƨA�
=A�"�A�5?A���A��
A���A�9XA�+A���A�p�A�^5A�K�A��A��hA���A�r�A�t�A�x�A� �A�=qA��7A�K�A��jA��PA��FA�ȴA��HA|jAx�Aw��Av�At�As33Aq�Ap�jAo?}AmC�AkƨAh�Ad��Aax�A^��A[AW�mAU;dASS�APĜAN5?AK�mAI�AG�AF�AD(�AB��A@z�A>jA=hsA<��A:9XA7VA49XA2v�A1��A0~�A/|�A.VA-oA+�#A*z�A*-A)�wA)&�A(�A'�^A&��A%�A%;dA$^5A#�PA"I�A!�A  �A��A�TA�-At�A��AM�AS�AVA�AO�A;dA�A�A�AK�A��A�A33A�A�RA�#AO�A+A��A�RAr�A�wA��A�A33A5?A�^A`BA
��A
��A
�A�A��A��AA+A=qA�wA��Al�A&�A=qAS�A ��A E�@�l�@�O�@���@���@��@��/@��@�C�@�$�@��j@�t�@�V@�`B@�u@�@��@�n�@�O�@�D@���@�~�@��@�K�@��@��/@�|�@�v�@�Z@��@�$�@ف@���@�I�@ׅ@�{@ԣ�@ӝ�@�o@�J@У�@Χ�@��@�G�@�Z@�K�@�n�@�G�@�r�@ǶF@�o@���@�1@�@��@���@�9X@�dZ@�{@�j@��@�S�@���@��T@�/@�b@��@�-@�hs@��j@��@�~�@���@��-@��7@�V@���@��D@���@���@��\@�v�@�$�@���@��@���@��F@�@��\@�-@���@�`B@��/@�1'@��;@��
@��;@��;@��@��
@�1'@���@��H@�^5@�O�@��9@�(�@�  @�(�@���@�+@�o@��@��\@�p�@��@���@�Q�@�9X@�b@��F@��;@�o@��@�G�@�&�@��@�&�@�J@�
=@���@���@�%@�1@�-@�?}@���@���@���@�r�@�I�@�(�@��m@�t�@�
=@��!@�M�@�@��T@��^@��7@�hs@�X@�G�@�/@�%@���@��@��`@���@���@��@�j@�Z@�Q�@�Q�@�bN@�r�@�r�@�Q�@� �@� �@�9X@���@���@��@�dZ@��@��@��j@���@�bN@�  @���@�|�@�@�~�@�ff@�ff@�V@�V@�M�@�$�@�-@��@��7@�&�@��`@���@�r�@�I�@� �@���@�t�@�o@��H@���@���@�n�@�=q@�@�@��h@�`B@�?}@�%@��/@�Ĝ@���@�r�@�(�@�  @�ƨ@��P@�33@�
=@��H@��R@�ff@�@��-@�x�@�7L@���@��j@��u@��@�j@�9X@��;@��@���@�l�@�33@�
=@���@��!@�~�@�V@�-@��@���@�?}@���@���@��j@���@��D@�j@�1'@��@��w@���@�K�@��@��!@�5?@�{@���@���@��@�X@���@��9@��@v5?@oK�@g\)@b�\@]p�@W
=@P��@J�@C33@;t�@4��@,�/@'�@#dZ@��@��@;d@C�@�R@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A˗�AˋDAˉ7AˍPA˙�A˝�A˙�A˗�A˟�Aˡ�A˅A�z�A�z�A�;dA�"�A�&�A�/A�K�A�E�A�?}A�7LA�-A�"�A� �A�{A��Aʟ�A�A�A�;dA�7LA�33A�/A� �A�A��Aɩ�AȰ!A�5?A�Q�A���A�A��DA���A�bA�|�A��!A�JA�ffA�?}A���A�G�A�+A��A�"�A��-A��A�dZA�A��hA�$�A��A�ĜA���A�ĜA��\A��A�A��DA��A�(�A��\A��-A���A���A�VA�bNA���A�I�A���A�O�A�%A��A�-A�VA��A��A�&�A��jA�1'A�ƨA�
=A�"�A�5?A���A��
A���A�9XA�+A���A�p�A�^5A�K�A��A��hA���A�r�A�t�A�x�A� �A�=qA��7A�K�A��jA��PA��FA�ȴA��HA|jAx�Aw��Av�At�As33Aq�Ap�jAo?}AmC�AkƨAh�Ad��Aax�A^��A[AW�mAU;dASS�APĜAN5?AK�mAI�AG�AF�AD(�AB��A@z�A>jA=hsA<��A:9XA7VA49XA2v�A1��A0~�A/|�A.VA-oA+�#A*z�A*-A)�wA)&�A(�A'�^A&��A%�A%;dA$^5A#�PA"I�A!�A  �A��A�TA�-At�A��AM�AS�AVA�AO�A;dA�A�A�AK�A��A�A33A�A�RA�#AO�A+A��A�RAr�A�wA��A�A33A5?A�^A`BA
��A
��A
�A�A��A��AA+A=qA�wA��Al�A&�A=qAS�A ��A E�@�l�@�O�@���@���@��@��/@��@�C�@�$�@��j@�t�@�V@�`B@�u@�@��@�n�@�O�@�D@���@�~�@��@�K�@��@��/@�|�@�v�@�Z@��@�$�@ف@���@�I�@ׅ@�{@ԣ�@ӝ�@�o@�J@У�@Χ�@��@�G�@�Z@�K�@�n�@�G�@�r�@ǶF@�o@���@�1@�@��@���@�9X@�dZ@�{@�j@��@�S�@���@��T@�/@�b@��@�-@�hs@��j@��@�~�@���@��-@��7@�V@���@��D@���@���@��\@�v�@�$�@���@��@���@��F@�@��\@�-@���@�`B@��/@�1'@��;@��
@��;@��;@��@��
@�1'@���@��H@�^5@�O�@��9@�(�@�  @�(�@���@�+@�o@��@��\@�p�@��@���@�Q�@�9X@�b@��F@��;@�o@��@�G�@�&�@��@�&�@�J@�
=@���@���@�%@�1@�-@�?}@���@���@���@�r�@�I�@�(�@��m@�t�@�
=@��!@�M�@�@��T@��^@��7@�hs@�X@�G�@�/@�%@���@��@��`@���@���@��@�j@�Z@�Q�@�Q�@�bN@�r�@�r�@�Q�@� �@� �@�9X@���@���@��@�dZ@��@��@��j@���@�bN@�  @���@�|�@�@�~�@�ff@�ff@�V@�V@�M�@�$�@�-@��@��7@�&�@��`@���@�r�@�I�@� �@���@�t�@�o@��H@���@���@�n�@�=q@�@�@��h@�`B@�?}@�%@��/@�Ĝ@���@�r�@�(�@�  @�ƨ@��P@�33@�
=@��H@��R@�ff@�@��-@�x�@�7L@���@��j@��u@��@�j@�9X@��;@��@���@�l�@�33@�
=@���@��!@�~�@�V@�-@��@���@�?}@���@���@��j@���@��D@�j@�1'@��@��w@���@�K�@��@��!@�5?@�{@���@���@��@�X@���@��9@��@v5?@oK�@g\)@b�\@]p�@W
=@P��@J�@C33@;t�@4��@,�/@'�@#dZ@��@��@;d@C�@�R@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�BB�B��BPB�B)�B-B2-BA�BT�BgmB�1BǮBBBB+B	7B�B0!B49B.B\B�B�oBL�B�mBǮBbB�hB��B��B��B��B��B��B��B�
B�B�B�fB�yB�B��B��B��B%B1B\B\BbBbB\BbBbBJB+BB��B�B�B�B�B�B�B�B�B�NB�BɺB�LB�B��B��B�VB�%Bx�BdZBS�B:^B�B�B��B�dB�!B�B�B��B��B��B�\B�%Bp�BK�B49B�BJB
�B
�^B
��B
|�B
\)B
E�B
%�B
PB
B	��B	�yB	�BB	��B	��B	�wB	�B	��B	�PB	v�B	W
B	9XB	$�B	VB	B��B��B�`B��BÖB�XB�!B�B��B��B��B��B�\B�DB�DB�VB�uB�oB�hB�JB�=B�7B�+B�%B�B�7B�DB�7B�%B�B�B~�Bz�Bw�Bu�Bv�Bt�Br�Bp�Bq�Br�Bq�Bp�Bq�Bq�Bo�Bn�Bn�Bm�BjBhsBffBdZBcTBaHB]/B[#B[#BZBZBYBXBVBS�BO�BO�BM�BM�BM�BL�BK�BJ�BG�BD�BA�B@�B>wB>wB=qB=qB=qB<jB;dB=qB:^B9XB8RB5?B2-B2-B49B33B2-B1'B1'B0!B.B-B,B+B(�B+B'�B$�B$�B#�B"�B#�B"�B#�B$�B$�B$�B#�B$�B%�B%�B&�B&�B$�B#�B%�B%�B%�B%�B%�B&�B(�B)�B+B-B/B/B1'B2-B2-B1'B2-B6FB6FB7LB;dB<jB>wBA�BF�BF�BH�BI�BK�BM�BQ�BVBW
BYBYB]/BdZBgmBgmBgmBiyBm�Bq�Bx�B}�B�B�%B�B�B�+B�=B�DB�JB�VB�hB�uB��B��B��B��B��B��B�B�B�9B�jB�qB�wB�}B�}B��BĜB��B�
B�B�B�#B�)B�5B�ZB�fB�fB�mB�sB�B�B��B��B��B	  B	B	1B	PB	{B	�B	 �B	�B	�B	�B	�B	�B	 �B	"�B	%�B	(�B	-B	/B	2-B	6FB	9XB	<jB	A�B	D�B	F�B	I�B	L�B	O�B	P�B	R�B	VB	[#B	^5B	aHB	e`B	hsB	k�B	l�B	n�B	o�B	p�B	q�B	t�B	v�B	w�B	w�B	x�B	z�B	� B	�B	�B	�+B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�9B	�FB	�LB	�XB	�dB	�jB	�jB	�jB	�qB	�qB	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
oB
�B
�B
%�B
.B
2-B
8RB
?}B
F�B
M�B
VB
\)B
^5B
e`B
ffB
l�B
o�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�/B�BB�B��BPB�B)�B.B2-BA�BVBiyB�DB��BBBB+B
=B�B2-B9XB:^B�B�B�dB|�B��BĜB+B��B��B�B�B��B��B�B�#B�ZB�BB�5B�sB�B�B��B  B+B\BbBoBuB�B�B{B�B�B�BoBPBDBB��B��B��B��B��B��B�B�B�mB��B�qB�9B��B��B��B�bB�%Br�BgmBR�B0!B��B�;B��B�-B�B�B�B�B��B��B��B�+B\)BA�B-B#�B�B
�B
�'B
��B
m�B
dZB
:^B
�B
VB
	7B	�B	�B	�/B	�B	��B	�qB	�FB	��B	�\B	m�B	P�B	:^B	�B	hB	\B	DB��B�`B��BÖBB�LB�?B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�7B�1B�VB�hB�bB�PB�=B�+B�%B�B�B~�B~�B}�Bw�Br�Bt�Bw�Bw�Bx�Bx�Bv�Bq�Bo�Bp�Bq�Bp�Bn�Bk�Bk�Bk�Bk�BffBaHB_;B\)B]/B\)B\)B]/B[#BXBVBT�BQ�BP�BP�BO�BP�BQ�BM�BJ�BG�BD�BD�BA�B?}B?}B@�BC�BD�B?}B=qB>wB=qB8RB6FB7LB8RB6FB5?B6FB6FB33B2-B1'B0!B1'B49B1'B)�B(�B'�B(�B+B(�B)�B)�B)�B+B,B)�B(�B(�B)�B+B(�B)�B+B%�B(�B+B,B-B,B)�B/B1'B33B/B1'B6FB6FB7LB8RB;dB6FB7LB;dB<jB>wBA�BF�BF�BH�BI�BO�BM�BQ�BVBZB\)B]/BaHBffBhsBhsBiyBk�Bn�Bu�B{�B~�B�B�%B�+B�%B�7B�VB�DB�VB�bB�uB��B��B��B��B��B��B��B�B�B�9B�wB��B��BÖB�}BBŢB��B�B�#B�#B�#B�)B�5B�ZB�fB�fB�mB�yB�B�B��B��B��B	  B	B	1B	DB	oB	 �B	#�B	"�B	#�B	$�B	�B	�B	!�B	#�B	&�B	(�B	.B	0!B	2-B	8RB	:^B	=qB	A�B	E�B	G�B	J�B	M�B	O�B	Q�B	R�B	W
B	[#B	^5B	aHB	ffB	hsB	k�B	m�B	n�B	o�B	p�B	q�B	t�B	v�B	x�B	w�B	x�B	z�B	�B	�B	�%B	�+B	�=B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�3B	�?B	�FB	�RB	�XB	�dB	�qB	�qB	�qB	�}B	�wB	B	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�HB	�BB	�HB	�HB	�NB	�ZB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
oB
�B
 �B
%�B
.B
2-B
8RB
?}B
G�B
M�B
VB
\)B
_;B
e`B
ffB
l�B
p�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�o<���=#�
=@�<�t�<#�
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
<49X<49X<�C�<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<���<ě�<ě�<���<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�9X<�o<T��<T��<�j=�w<���<ě�<ě�<�C�<�<��
<#�
<#�
<e`B<#�
<#�
<#�
<49X<u<u<�j<���<ě�<�9X<�j<�1<�C�<�o<���<�C�<�C�<�o<#�
<#�
<�t�<D��<�o<T��<#�
<#�
<�t�<�9X<�C�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452202012011014522020120110145220  AO  ARGQ                                                                        20111130144316  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144316  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145220  IP                  G�O�G�O�G�O�                