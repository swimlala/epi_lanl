CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:53Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               5A   AO  20111130144034  20190522121829  1728_5048_053                   2C  D   APEX                            2142                            040306                          846 @Ԯ�w���1   @Ԯ�""(@5������c+I�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B���B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy�fD� D�9�D�FfD��fD�3D�&fD�ffD��fD���D�,�D�0 DǦfD���D�#3D�y�D��D��3D�&fD�L�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @�  @�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffB��BffBffB&ffB.ffB6ffB>ffBFffBNffBV  B^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�  B�  B�33B�33B�33C��C��C��C��C	��C��C� C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD��D	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5� D6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfl�Df�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDw` Dy��D�3D�,�D�9�D���D��fD��D�Y�D���D���D�  D�#3DǙ�D�� D�fD�l�D� D��fD��D�@ D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A¥�A¡�A�A©�A²-A²-A²-A´9A´9A²-A´9A¶FA¸RA¸RA¸RAº^A¼jA¼jAº^A¸RA´9A¶FA¶FA¶FA�v�A���A��uA�"�A���A���A��RA��9A��FA���A���A���A��uA��\A��7A��A��A�z�A�l�A�l�A�v�A��A��A��A�~�A�|�A�v�A�r�A�r�A�t�A�p�A�ffA�S�A�VA��mA���A�G�A��A�ĜA���A��hA��^A��-A���A�ĜA�A�M�A�?}A��7A�%A�Q�A�Q�A�t�A��RA��uA���A���A��A��A���A�A�A�bA���A�z�A�(�A��^A���A��A��\A��A��A��#A�K�A�^5A�n�A�I�A��A��+A�hsA�l�A��^A���A�v�A�\)A��
A�33A�ȴA��RA��TA�%A�#A}p�A{��Ay��AwAt�As33Arv�Aq�;An�`Ak��Ai?}Af�Ae�Ac�AbffAa"�A^��A[�^AY��AWl�AS�ARffAQG�AO;dAMVAK�TAI+AG�AF�HAEAD=qACO�AB~�ABAA&�A?�A>-A;�
A8�RA8�A6��A5|�A4A2��A21'A0��A/�mA.��A-��A,��A,E�A+ƨA*��A*  A)��A)%A'��A&��A%dZA$ffA#�wA#G�A"�A"�A!`BA ��AA��A{A�9AA��A��A�-A&�Az�AƨA33Av�AS�A5?A��A"�Av�At�Av�A`BA-AC�A
�A	�A�A  AVA(�AK�A��A��At�A  A �u@�V@�Ĝ@�;d@�M�@��/@� �@�C�@��D@��@�Ĝ@��@�J@�7L@�1@�;d@�hs@䛦@��H@�^@�V@ߕ�@ݡ�@�1@��@��@�o@�^5@�/@�K�@җ�@�J@�?}@��
@�ff@�Q�@�K�@�M�@ȴ9@�33@�G�@ă@�A�@� �@ÍP@��@���@� �@��y@���@��@�hs@�r�@���@�"�@��R@���@��@��@�bN@��@���@�5?@���@�X@���@���@�r�@� �@��@���@���@�hs@��/@�  @���@��R@�~�@�ff@�5?@�J@���@�?}@� �@��@�E�@�?}@��/@��9@��@�+@�v�@�5?@��#@�hs@�Ĝ@�A�@��
@���@���@�|�@�"�@��@��y@��H@��!@��@��#@���@���@��@�r�@���@�dZ@�"�@���@���@���@��!@���@��y@�@�^5@���@�7L@���@��@�r�@�A�@��@�1@��@��;@��w@���@��P@�S�@�+@�o@�@��y@�ȴ@���@�^5@�E�@�{@��#@���@��@�G�@�G�@�X@�`B@�hs@�hs@�hs@�?}@�V@��u@��;@��F@���@���@��P@�K�@�
=@��@��\@�{@��^@��@� �@�ƨ@��@���@�S�@�"�@�\)@���@�9X@�A�@�A�@���@�ƨ@�ƨ@��w@��@��@�ƨ@�ƨ@��P@�K�@�
=@��@���@�$�@���@�`B@�7L@���@��@�9X@�  @��w@���@��P@��@�l�@�;d@�@��y@���@�ff@��T@���@��-@��7@�G�@��@�V@�V@�V@���@��@�z�@��@�1@��m@��
@���@�"�@���@�ff@�5?@�@��@���@���@��F@��
@�1'@�  @��@�S�@�C�@���@�~�@�-@�$�@�J@��h@�7L@��@�V@�%@��/@�j@��@��m@�9X@���@��u@�Z@���@�ƨ@���@�\)@�33@��@���@���@���@���@�n�@�=q@��@��^@��@�hs@�Ĝ@{�
@r�H@iX@`�u@Z=q@St�@L(�@E?}@<�D@5p�@/�@*�@&V@!%@�@��@{@-@O�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A¥�A¡�A�A©�A²-A²-A²-A´9A´9A²-A´9A¶FA¸RA¸RA¸RAº^A¼jA¼jAº^A¸RA´9A¶FA¶FA¶FA�v�A���A��uA�"�A���A���A��RA��9A��FA���A���A���A��uA��\A��7A��A��A�z�A�l�A�l�A�v�A��A��A��A�~�A�|�A�v�A�r�A�r�A�t�A�p�A�ffA�S�A�VA��mA���A�G�A��A�ĜA���A��hA��^A��-A���A�ĜA�A�M�A�?}A��7A�%A�Q�A�Q�A�t�A��RA��uA���A���A��A��A���A�A�A�bA���A�z�A�(�A��^A���A��A��\A��A��A��#A�K�A�^5A�n�A�I�A��A��+A�hsA�l�A��^A���A�v�A�\)A��
A�33A�ȴA��RA��TA�%A�#A}p�A{��Ay��AwAt�As33Arv�Aq�;An�`Ak��Ai?}Af�Ae�Ac�AbffAa"�A^��A[�^AY��AWl�AS�ARffAQG�AO;dAMVAK�TAI+AG�AF�HAEAD=qACO�AB~�ABAA&�A?�A>-A;�
A8�RA8�A6��A5|�A4A2��A21'A0��A/�mA.��A-��A,��A,E�A+ƨA*��A*  A)��A)%A'��A&��A%dZA$ffA#�wA#G�A"�A"�A!`BA ��AA��A{A�9AA��A��A�-A&�Az�AƨA33Av�AS�A5?A��A"�Av�At�Av�A`BA-AC�A
�A	�A�A  AVA(�AK�A��A��At�A  A �u@�V@�Ĝ@�;d@�M�@��/@� �@�C�@��D@��@�Ĝ@��@�J@�7L@�1@�;d@�hs@䛦@��H@�^@�V@ߕ�@ݡ�@�1@��@��@�o@�^5@�/@�K�@җ�@�J@�?}@��
@�ff@�Q�@�K�@�M�@ȴ9@�33@�G�@ă@�A�@� �@ÍP@��@���@� �@��y@���@��@�hs@�r�@���@�"�@��R@���@��@��@�bN@��@���@�5?@���@�X@���@���@�r�@� �@��@���@���@�hs@��/@�  @���@��R@�~�@�ff@�5?@�J@���@�?}@� �@��@�E�@�?}@��/@��9@��@�+@�v�@�5?@��#@�hs@�Ĝ@�A�@��
@���@���@�|�@�"�@��@��y@��H@��!@��@��#@���@���@��@�r�@���@�dZ@�"�@���@���@���@��!@���@��y@�@�^5@���@�7L@���@��@�r�@�A�@��@�1@��@��;@��w@���@��P@�S�@�+@�o@�@��y@�ȴ@���@�^5@�E�@�{@��#@���@��@�G�@�G�@�X@�`B@�hs@�hs@�hs@�?}@�V@��u@��;@��F@���@���@��P@�K�@�
=@��@��\@�{@��^@��@� �@�ƨ@��@���@�S�@�"�@�\)@���@�9X@�A�@�A�@���@�ƨ@�ƨ@��w@��@��@�ƨ@�ƨ@��P@�K�@�
=@��@���@�$�@���@�`B@�7L@���@��@�9X@�  @��w@���@��P@��@�l�@�;d@�@��y@���@�ff@��T@���@��-@��7@�G�@��@�V@�V@�V@���@��@�z�@��@�1@��m@��
@���@�"�@���@�ff@�5?@�@��@���@���@��F@��
@�1'@�  @��@�S�@�C�@���@�~�@�-@�$�@�J@��h@�7L@��@�V@�%@��/@�j@��@��m@�9X@���@��u@�Z@���@�ƨ@���@�\)@�33@��@���@���@���@���@�n�@�=q@��@��^@��@�hs@�Ĝ@{�
@r�H@iX@`�u@Z=q@St�@L(�@E?}@<�D@5p�@/�@*�@&V@!%@�@��@{@-@O�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B8RB|�B�-B�?B�-B�!B�!B�-B�3B�-B�3B�9B�?B�FB�FB�FB�?B�9B�?B�LB�XB�^B�^B�^B�^B�^B�^B�^B�^B�dB�^B�jB�XB�9B�-BĜBƨB��B�wB�^B�'B�B��B��B��B�uB�\B�PB�JB�7B�Bz�Bs�Bm�Bn�BbNBW
BK�BI�B9XB�BuB{BoBPB��B�yB�sB�HB�#B��B�B��B��B�oBk�BT�BB�B.B �BJB
��B
�;B
ƨB
��B
�1B
w�B
jB
N�B
O�B
9XB
2-B
)�B
 �B
B	��B	��B	�fB	��B	�^B	�B	��B	�{B	�JB	z�B	v�B	bNB	O�B	33B	�B	�B	B��B�B�B�BɺB��B�B�B��B��B��B��BȴB�jB�?B��B��B��B�\B�PB�DB�%B�B� B}�Bx�Bx�Bw�Bw�Bw�Bw�Bx�Bv�Bs�Bq�Bs�Bw�Bo�Bo�Bo�Bo�Bn�BjBjBl�BiyBiyBcTBcTB`BB^5B^5B^5B]/B]/B]/B\)B[#B[#BZB[#BZBT�BVBP�BN�BL�BJ�BG�BF�BE�BD�BE�BD�BA�BB�BA�B@�BD�BC�BE�BG�B:^B<jB>wB=qB6FB7LB6FB49B6FB33B8RB2-B5?B49B2-B+B+B7LB49B.B-B+B-B0!B5?B5?B49B2-B/B.B)�B&�B%�B+B-B'�B+B)�B+B,B+B(�B)�B,B49B<jB@�BE�BL�BN�BN�BN�BP�BP�BP�BS�BT�B[#BW
BYB\)B\)B\)B^5B]/B[#B]/Be`BffBjBr�Bw�Bv�Bw�Bw�Bw�By�B� B�B�1B�DB�uB��B��B��B��B��B�B�B�'B�9B�^B��BÖBƨBǮB��B��B��B��B�#B�;B�fB�yB�yB�B�B�B�B�B��B��B��B��B��B��B��B	B		7B	DB	1B	
=B	bB	uB	�B	�B	�B	�B	�B	�B	 �B	#�B	&�B	(�B	+B	,B	.B	0!B	49B	7LB	8RB	<jB	B�B	F�B	I�B	O�B	T�B	XB	[#B	^5B	bNB	hsB	jB	l�B	o�B	s�B	u�B	v�B	v�B	v�B	y�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�'B	�'B	�9B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	��B	B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�;B	�;B	�BB	�;B	�BB	�HB	�NB	�BB	�NB	�`B	�`B	�`B	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
#�B
)�B
0!B
8RB
>wB
C�B
K�B
R�B
XB
]/B
aHB
ffB
jB
m�B
q�B
v�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B>wB�B�3B�FB�3B�!B�!B�-B�3B�-B�3B�9B�?B�FB�FB�FB�?B�9B�?B�LB�XB�^B�^B�^B�^B�^B�^B�^B�^B�dB�^B�jB�^B�dB��B��BȴBBÖBÖB�FB�'B�B��B��B��B�uB�bB�bB�VB�B}�Bw�Bq�Bq�BffBYBN�BM�B?}B�B{B�B{BoBB�B�B�ZB�TBɺB�!B��B��B��Br�B[#BG�B2-B&�BuB
��B
�sB
��B
�B
�VB
|�B
u�B
T�B
W
B
=qB
7LB
.B
&�B
B	��B	��B	�B	�/B	��B	�3B	��B	��B	�bB	~�B	|�B	k�B	VB	:^B	#�B	�B		7B��B�B�B�HB��B��B�)B�5B�B��B��B��B��B��B�qB�B��B��B�{B�oB�\B�7B�=B�B�B|�B{�By�By�Bz�B{�Bz�By�Bw�Bv�Bx�Bz�Bq�Bq�Bp�Bq�Bp�Bk�Bm�Bn�Bl�Bm�BhsBgmBcTBaHB`BB`BB_;B_;B`BB`BB_;B]/B\)B^5B]/BXBZBT�BQ�BP�BN�BI�BH�BI�BH�BH�BG�BD�BE�BG�BE�BI�BF�BH�BI�B<jB=qBD�BA�B8RB:^B9XB6FB7LB5?B9XB5?B6FB7LB49B,B.B:^B7LB0!B0!B.B.B2-B8RB6FB5?B49B2-B1'B.B(�B'�B.B0!B+B-B+B,B.B.B+B,B.B5?B>wBA�BG�BL�BO�BO�BP�BR�BP�BQ�BT�BVB\)BXBZB]/B]/B]/B_;B^5B]/B_;BffBgmBl�Bt�Bx�Bw�Bx�Bx�Bx�Bz�B�B�B�7B�PB��B��B��B��B��B��B�B�B�-B�?B�dBBÖBƨBȴB��B��B��B��B�)B�HB�fB�yB�yB�B�B�B�B�B��B��B��B��B��B��B��B	B	
=B	JB		7B	DB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	$�B	'�B	(�B	+B	,B	.B	1'B	5?B	7LB	9XB	=qB	C�B	F�B	J�B	O�B	T�B	XB	[#B	^5B	bNB	iyB	k�B	m�B	p�B	t�B	u�B	v�B	v�B	w�B	z�B	~�B	�B	�B	�%B	�%B	�%B	�B	�B	�B	�+B	�=B	�DB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�B	�B	�-B	�-B	�?B	�?B	�?B	�LB	�RB	�RB	�^B	�dB	�dB	�qB	�wB	��B	B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�5B	�;B	�;B	�HB	�BB	�BB	�NB	�TB	�HB	�NB	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
#�B
+B
0!B
9XB
>wB
C�B
K�B
R�B
XB
]/B
aHB
gmB
jB
n�B
q�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452112012011014521120120110145211  AO  ARGQ                                                                        20111130144034  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144034  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145211  IP                  G�O�G�O�G�O�                