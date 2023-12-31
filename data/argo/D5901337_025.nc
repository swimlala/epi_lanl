CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:30Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112742  20190522121836  1901_5055_025                   2C  D   APEX                            2140                            040306                          846 @�g{=p��1   @�g{�s��@+������cz��+1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�33B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dy� D��D�9�D�c3D��3D�fD�@ D��fD��3D�fD�33D�|�D��3D��fD�,�D�` D�� D��3D�  D�i�D�Vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6��B>��BFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B���B�33B�33B�33B�33B�ffB�  B�  B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�33C��C��C��C��C	��C��C��C��C��C��C��C��C� C��C��C��C!��C#�3C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE� CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{�3C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fDffD� D ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDo` Do�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyffD�  D�,�D�VfD��fD���D�33D�y�D��fD���D�&fD�p DǶfD��D�  D�S3D�3D��fD�3D�\�D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�?}A��`A̧�A̓uA�t�A�dZA�ZA�M�A�9XA�&�A��A�1A��A��A�O�A�Q�A��A��#Aɲ-A�`BA�9XA�$�A���A���Aȣ�Aȏ\AȋDA�r�A�1'A��
Aǉ7A�$�A��`Aƺ^Aƛ�A�`BA�K�A�I�A�G�A�A�A�5?A�(�A��A��+A���A�z�A�E�A���A��+A��^A�M�A�Q�A��+A��-A���A��RA�?}A�|�A��PA�&�A�{A���A�ĜA��A���A��DA�~�A�v�A�"�A�jA�v�A���A���A�{A�v�A��;A�|�A�O�A��-A�S�A�?}A��A���A�dZA�+A���A���A�O�A�9XA�$�A��+A�Q�A�\)A��/Aw\)AqVAh�!AbM�A]p�AY�AW�AU��AQ�#AK�AI�
AI7LAF�9AEAE�7AD��AC�hA@�/A=��A<bNA:��A5�TA5`BA3�A0�`A/&�A.v�A-�-A,�`A+�A+?}A*��A*�A+�A*��A*E�A)�A)��A*1A)�A)�mA)��A(v�A'�7A%��A#�#A"�A"�+A"M�A"5?A �`A =qAO�A��AbA�AĜA �A��AhsA?}A1'A��A^5A1'Az�A/A��AO�A��A�A��A��A=qA��Av�AA�A�RA7LA�A�jA��Av�AffAE�A  A��A�AA��A�An�AbNAr�AbA�7A%A��A�
AK�A/A�A��AȴAv�AM�A5?A-A �AJA?}A
�A
 �A	ƨA	7LA��A��AI�A�
A7LA�jAn�A1'A��A�A�\A5?A��Av�A��A ȴA 1@���@�K�@���@�$�@���@��-@���@���@��@��@�"�@��+@�"�@�^5@�1'@��@�n�@���@� �@�@��@陚@���@�r�@�t�@�K�@�ȴ@��@��@�/@�P@㝲@��@�-@��@ᙚ@ᙚ@�h@�/@�%@�V@�V@��@�Ĝ@�bN@��;@��H@�p�@�O�@�&�@���@۝�@�
=@ڟ�@��@�?}@���@ؓu@ԃ@�M�@�I�@���@�hs@�I�@�;d@���@�-@�J@���@ɡ�@�?}@�Z@ȓu@�&�@��@�9X@�C�@��@ȋD@ǍP@�E�@Ų-@���@ēu@�r�@�I�@�b@�1@�  @��@å�@�+@���@§�@�ff@���@�7L@�I�@�(�@�ƨ@��P@�;d@��y@���@�ff@�=q@��@�p�@�%@���@��@��@�Q�@��@�\)@�o@�~�@���@��7@�G�@��`@�I�@�l�@���@�=q@��@��7@�`B@�?}@�%@��@�Q�@�(�@�  @���@�|�@�C�@���@�n�@��@��#@���@�x�@�G�@�Ĝ@�9X@���@�t�@�K�@��R@�^5@�-@��@�{@��T@�`B@���@�(�@�ƨ@�\)@���@���@��h@��9@�j@�bN@�A�@��@�$�@��^@�?}@��/@��@�j@� �@��m@��@��@�v�@�M�@�@�&�@��@�Q�@�  @�33@���@��@���@��\@�n�@�M�@�$�@��T@��@���@��D@�r�@�Q�@�A�@� �@��
@��@�K�@�ȴ@���@�V@��#@�@��-@��-@���@��7@�hs@�O�@��@���@�Ĝ@��D@�Z@�Q�@�I�@�9X@�A�@�9X@��@��@��;@���@��F@�|�@�;d@��@��y@��@��@��R@��\@�n�@�V@��T@�x�@�V@���@�1'@���@�S�@�+@��@���@�~�@�ff@�=q@��@�@�p�@�?}@�&�@���@��/@���@���@�I�@��
@��P@��T@��@��@zM�@pbN@ix�@a��@W�;@PA�@E@>�R@6V@0A�@+"�@$��@!��@�@��@��@z�@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�?}A��`A̧�A̓uA�t�A�dZA�ZA�M�A�9XA�&�A��A�1A��A��A�O�A�Q�A��A��#Aɲ-A�`BA�9XA�$�A���A���Aȣ�Aȏ\AȋDA�r�A�1'A��
Aǉ7A�$�A��`Aƺ^Aƛ�A�`BA�K�A�I�A�G�A�A�A�5?A�(�A��A��+A���A�z�A�E�A���A��+A��^A�M�A�Q�A��+A��-A���A��RA�?}A�|�A��PA�&�A�{A���A�ĜA��A���A��DA�~�A�v�A�"�A�jA�v�A���A���A�{A�v�A��;A�|�A�O�A��-A�S�A�?}A��A���A�dZA�+A���A���A�O�A�9XA�$�A��+A�Q�A�\)A��/Aw\)AqVAh�!AbM�A]p�AY�AW�AU��AQ�#AK�AI�
AI7LAF�9AEAE�7AD��AC�hA@�/A=��A<bNA:��A5�TA5`BA3�A0�`A/&�A.v�A-�-A,�`A+�A+?}A*��A*�A+�A*��A*E�A)�A)��A*1A)�A)�mA)��A(v�A'�7A%��A#�#A"�A"�+A"M�A"5?A �`A =qAO�A��AbA�AĜA �A��AhsA?}A1'A��A^5A1'Az�A/A��AO�A��A�A��A��A=qA��Av�AA�A�RA7LA�A�jA��Av�AffAE�A  A��A�AA��A�An�AbNAr�AbA�7A%A��A�
AK�A/A�A��AȴAv�AM�A5?A-A �AJA?}A
�A
 �A	ƨA	7LA��A��AI�A�
A7LA�jAn�A1'A��A�A�\A5?A��Av�A��A ȴA 1@���@�K�@���@�$�@���@��-@���@���@��@��@�"�@��+@�"�@�^5@�1'@��@�n�@���@� �@�@��@陚@���@�r�@�t�@�K�@�ȴ@��@��@�/@�P@㝲@��@�-@��@ᙚ@ᙚ@�h@�/@�%@�V@�V@��@�Ĝ@�bN@��;@��H@�p�@�O�@�&�@���@۝�@�
=@ڟ�@��@�?}@���@ؓu@ԃ@�M�@�I�@���@�hs@�I�@�;d@���@�-@�J@���@ɡ�@�?}@�Z@ȓu@�&�@��@�9X@�C�@��@ȋD@ǍP@�E�@Ų-@���@ēu@�r�@�I�@�b@�1@�  @��@å�@�+@���@§�@�ff@���@�7L@�I�@�(�@�ƨ@��P@�;d@��y@���@�ff@�=q@��@�p�@�%@���@��@��@�Q�@��@�\)@�o@�~�@���@��7@�G�@��`@�I�@�l�@���@�=q@��@��7@�`B@�?}@�%@��@�Q�@�(�@�  @���@�|�@�C�@���@�n�@��@��#@���@�x�@�G�@�Ĝ@�9X@���@�t�@�K�@��R@�^5@�-@��@�{@��T@�`B@���@�(�@�ƨ@�\)@���@���@��h@��9@�j@�bN@�A�@��@�$�@��^@�?}@��/@��@�j@� �@��m@��@��@�v�@�M�@�@�&�@��@�Q�@�  @�33@���@��@���@��\@�n�@�M�@�$�@��T@��@���@��D@�r�@�Q�@�A�@� �@��
@��@�K�@�ȴ@���@�V@��#@�@��-@��-@���@��7@�hs@�O�@��@���@�Ĝ@��D@�Z@�Q�@�I�@�9X@�A�@�9X@��@��@��;@���@��F@�|�@�;d@��@��y@��@��@��R@��\@�n�@�V@��T@�x�@�V@���@�1'@���@�S�@�+@��@���@�~�@�ff@�=q@��@�@�p�@�?}@�&�@���@��/@���@���@�I�@��
@��P@��T@��@��@zM�@pbN@ix�@a��@W�;@PA�@E@>�R@6V@0A�@+"�@$��@!��@�@��@��@z�@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	jB	jB	jB	jB	k�B	k�B	k�B	k�B	jB	iyB	hsB	gmB	e`B	aHB	aHB	S�B	H�B	I�B	J�B	J�B	N�B	P�B	R�B	VB	YB	_;B	e`B	ffB	iyB	u�B	�+B	��B	�B	�}B	��B	��B	�HB	�`B	�fB	�mB	�B	�B	��B
��B
ǮB
��BL�B�bB�3B�^B�ZB  B-B7LB.B33B33BO�B�bB�=B�\B�oB��B��B��B��B��B��B��B��B��B��B�BjBXBK�BC�B8RB�BVB1B%B��B�mB��B�-B�bBiyB7LB\B
�`B
�
B
�LB
A�B	�BB	x�B	J�B	�B��B�B�5B�
B��BB�XB�FB�-B�!B�'B�!B�-B��B�TB�B�B�/B�NB�HB�B��B��B��B	B	DB	�B	�B	�B	'�B	:^B	=qB	;dB	>wB	K�B	hsB	u�B	�=B	��B	��B	��B	�uB	�=B	�1B	�+B	�+B	�%B	}�B	�B	�B	r�B	bNB	W
B	O�B	49B	$�B	>wB	A�B	YB	hsB	M�B	S�B	_;B	r�B	�oB	�B	u�B	~�B	�VB	�{B	��B	�B	�B	�3B	�}B	��B	��B	�
B	�B	�B	�B	�)B	�;B	�BB	�fB	�yB	�B	�B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B

=B

=B
JB
JB
PB
bB
bB
hB
hB
hB
\B
VB
VB
PB
VB
\B
JB
JB
+B
B
B
  B
1B

=B

=B

=B
DB

=B
	7B
%B
B	��B
B
B

=B
1B
  B	��B	��B	�B	�;B	�#B	�#B	�/B	�NB	�;B	�BB	�HB	�NB	�NB	�TB	�`B	�TB	�fB	�ZB	�TB	�TB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�`B	��B	��B	ĜB	�qB	�FB	�9B	�'B	�B	�B	�-B	�9B	�FB	�FB	�LB	�dB	��B	ÖB	ƨB	ȴB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�/B	�/B	�5B	�5B	�)B	�)B	�5B	�;B	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�yB	�sB	�mB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
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
B
B
B
B
%B
+B
+B
1B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
+B
1B
1B

=B
DB
DB
PB
\B
bB
bB
bB
bB
bB
\B
VB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
hB
oB
�B
�B
�B
�B
�B
'�B
2-B
7LB
<jB
B�B
F�B
K�B
P�B
YB
\)B
cTB
hsB
k�B
o�B
q�B
v�B
{�B
�B
�%B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	k�B	l�B	k�B	k�B	l�B	l�B	k�B	k�B	k�B	jB	iyB	hsB	ffB	bNB	e`B	YB	J�B	J�B	K�B	L�B	O�B	Q�B	S�B	W
B	ZB	`BB	e`B	gmB	jB	w�B	�1B	��B	�!B	��B	��B	��B	�NB	�`B	�fB	�mB	�B	�B
B
��B
ǮB
�
BVB��B�RB��B�`BB0!B:^B33B8RB6FBN�B��B�JB�bB�uB��B��B��B��B��B��B��B��B��B��B�1Bn�B[#BN�BF�BF�B�BhB	7BDBB�B��B�^B��Bt�B?}B�B
�mB
�#B
ɺB
T�B	��B	�1B	aHB	+B	
=B��B�ZB�;B�BB�B��B�^B�dB�9B�-B�3B�LB�B�B�)B�5B�yB�ZB�`B��B��B��B	B	%B	VB	�B	�B	�B	'�B	;dB	?}B	<jB	?}B	J�B	hsB	u�B	�DB	��B	��B	��B	��B	�PB	�7B	�1B	�1B	�=B	� B	�+B	�%B	w�B	e`B	YB	XB	>wB	"�B	?}B	?}B	[#B	s�B	N�B	R�B	]/B	n�B	��B	�DB	u�B	}�B	�VB	�uB	��B	�B	�!B	�-B	�wB	��B	��B	�B	�B	�B	�#B	�/B	�BB	�HB	�fB	�B	�B	�B	��B	��B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
JB
JB
JB
PB
VB
VB
hB
oB
uB
uB
uB
bB
\B
bB
bB
\B
bB
VB
bB

=B
1B
+B
B
	7B
DB
DB

=B
JB
JB
JB
	7B
B	��B
B
B
JB
DB
B	��B	��B	�B	�NB	�)B	�#B	�/B	�ZB	�HB	�BB	�NB	�ZB	�TB	�TB	�mB	�TB	�sB	�`B	�ZB	�TB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	ƨB	��B	�RB	�FB	�-B	�!B	�B	�3B	�?B	�LB	�RB	�LB	�^B	��B	ŢB	ȴB	ƨB	�B	�B	�
B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�BB	�5B	�)B	�;B	�BB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�mB	�fB	�fB	�sB	�yB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�yB	�sB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
+B
+B
1B
	7B
1B
1B
1B
1B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
1B
	7B
	7B

=B
DB
JB
VB
\B
hB
hB
hB
hB
hB
bB
\B
PB
PB
PB
VB
VB
PB
VB
\B
\B
bB
bB
bB
hB
bB
hB
oB
�B
�B
�B
�B
�B
'�B
2-B
7LB
<jB
C�B
F�B
K�B
Q�B
YB
\)B
cTB
hsB
l�B
p�B
q�B
w�B
{�B
�B
�%B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<�t�<���<ě�<u<�1<�t�<u<e`B<#�
<#�
<�o<�9X<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250162012011312501620120113125016  AO  ARGQ                                                                        20111205112742  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112742  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125016  IP                  G�O�G�O�G�O�                