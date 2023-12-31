CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:41Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               >A   AO  20111205113425  20190522121836  1901_5055_062                   2C  D   APEX                            2140                            040306                          846 @��dTb�1   @��e`��@-�;dZ�c�
=p��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1�fD2fD2�fD3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv� Dyy�D� D�33D�|�D��fD� D�)�D�� D���D��D�33D�� Dǰ D��D�0 D�s3D��D��fD�33D�Vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.��B6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!�3C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE� CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD � D!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0��D1l�D1��D2l�D2��D3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ� DKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo��Dpl�Dp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDy` D�3D�&fD�p D���D�3D��D��3D�� D���D�&fD�s3Dǣ3D���D�#3D�ffD� D��D�&fD�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�;dA�=qA�?}A�E�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�K�A�M�A�O�A�O�A�O�A�K�A�K�A�M�A�A�A�-A��A��A���AƩ�A��HA��#Aå�A�$�A���A��mA�A���A�M�A���A��A�9XA��A��#A��wA��^A��jA��FA�p�A�\)A�n�A��`A��FA��uA���A��uA�-A��A�A���A��
A�K�A�x�A��^A���A�  A�~�A�bNA��yA�dZA��mA�dZA�"�A�+A��A���A���A�E�A�{A�ĜA�%A�^5A���A��HA��/A���A��A��+A��mA�p�A�G�A��PA��A���A���A�+A��A�=qA��-A�v�A��PA}�7Aw|�Aq�;Ak��Ae��Ab  A_XA]�^AV�\APQ�ALQ�AIK�AG\)AF  AC�PAB�/A@�A=��A;G�A9�FA7�A6jA5�7A2�\A.ȴA-7LA* �A&��A%�TA%�7A%
=A$ȴA$jA#��A#�A"��A"�A!hsA"bA#�A#33A#+A!�hA�7A`BA��A�7A�;A�A �A�TAp�A�
A z�A (�A�hA �\A �+A ��A �uA =qA bA��AdZAt�AO�A�jA9XA��AhsAA1'A�;AdZAA9XA-AƨA�A�RA�A�FA��A\)A�AM�A|�A��A �A$�A�#AhsA;dA�Av�A-AAp�A��AE�A�A��AbNAJA  A�A��A�A�AA�AJAAƨA
�A
Q�A
JA	�A	�hA	S�A�`AQ�A�A�A�A�A��A�A�yAVA�A�#AXA"�A�RAjAI�A(�A��A�A��A��AdZAC�AoAA �HA �yA ĜA bNA {@��m@�ƨ@��@�$�@�%@�I�@�1@���@��@��w@�dZ@��@���@��@�7L@��/@��@���@��#@�D@�S�@�ƨ@�1@�|�@�t�@�!@��#@�@��;@��@��^@�z�@ꟾ@�{@��@�1@畁@�P@�t�@�@噚@�&�@���@��`@��`@���@�Q�@��@��H@��@�Ĝ@�M�@���@݉7@�`B@�V@ܬ@�Ĝ@�G�@���@ݑh@��@ܬ@��@�@�5?@٩�@�hs@�&�@���@��H@�ff@պ^@�Q�@�ƨ@ӝ�@�+@��H@ҏ\@�=q@��@Ѳ-@�X@��@���@д9@�bN@��@���@�dZ@�"�@�@θR@ͩ�@�`B@�G�@�I�@˝�@�dZ@��H@���@�`B@���@ȓu@�b@Ǯ@�C�@�^5@Ł@�z�@�I�@�A�@�(�@�  @ÍP@��H@\@�n�@�M�@�-@��#@�`B@��@���@�r�@��@���@�M�@��T@��h@�X@���@��9@�A�@�dZ@���@�-@���@���@�1@��P@�;d@��@��R@���@��@�?}@��@��D@�Z@�9X@��@�~�@��@���@�hs@�7L@��`@�Q�@���@���@�C�@���@�=q@�hs@��`@�1'@���@�dZ@�o@��@���@�=q@���@���@��/@���@�(�@�|�@�C�@��H@��+@�-@��@��^@��7@��@��9@�Q�@�(�@��@�ƨ@�33@��!@�v�@�{@�@��7@�G�@��@��@���@��@� �@�|�@���@�^5@�5?@��@�p�@��@��D@���@��P@�t�@�33@���@���@�x�@�%@�Q�@��m@�t�@��@��@�ȴ@�~�@�=q@��@�`B@���@��j@�I�@��@��;@���@�t�@�33@��H@�~�@�V@�{@���@��h@�x�@�b@�`B@���@}?}@uV@k"�@_l�@W�@P �@H�u@?
=@6�R@.�y@*J@%p�@!�#@��@V@��@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�;dA�=qA�?}A�E�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�K�A�M�A�O�A�O�A�O�A�K�A�K�A�M�A�A�A�-A��A��A���AƩ�A��HA��#Aå�A�$�A���A��mA�A���A�M�A���A��A�9XA��A��#A��wA��^A��jA��FA�p�A�\)A�n�A��`A��FA��uA���A��uA�-A��A�A���A��
A�K�A�x�A��^A���A�  A�~�A�bNA��yA�dZA��mA�dZA�"�A�+A��A���A���A�E�A�{A�ĜA�%A�^5A���A��HA��/A���A��A��+A��mA�p�A�G�A��PA��A���A���A�+A��A�=qA��-A�v�A��PA}�7Aw|�Aq�;Ak��Ae��Ab  A_XA]�^AV�\APQ�ALQ�AIK�AG\)AF  AC�PAB�/A@�A=��A;G�A9�FA7�A6jA5�7A2�\A.ȴA-7LA* �A&��A%�TA%�7A%
=A$ȴA$jA#��A#�A"��A"�A!hsA"bA#�A#33A#+A!�hA�7A`BA��A�7A�;A�A �A�TAp�A�
A z�A (�A�hA �\A �+A ��A �uA =qA bA��AdZAt�AO�A�jA9XA��AhsAA1'A�;AdZAA9XA-AƨA�A�RA�A�FA��A\)A�AM�A|�A��A �A$�A�#AhsA;dA�Av�A-AAp�A��AE�A�A��AbNAJA  A�A��A�A�AA�AJAAƨA
�A
Q�A
JA	�A	�hA	S�A�`AQ�A�A�A�A�A��A�A�yAVA�A�#AXA"�A�RAjAI�A(�A��A�A��A��AdZAC�AoAA �HA �yA ĜA bNA {@��m@�ƨ@��@�$�@�%@�I�@�1@���@��@��w@�dZ@��@���@��@�7L@��/@��@���@��#@�D@�S�@�ƨ@�1@�|�@�t�@�!@��#@�@��;@��@��^@�z�@ꟾ@�{@��@�1@畁@�P@�t�@�@噚@�&�@���@��`@��`@���@�Q�@��@��H@��@�Ĝ@�M�@���@݉7@�`B@�V@ܬ@�Ĝ@�G�@���@ݑh@��@ܬ@��@�@�5?@٩�@�hs@�&�@���@��H@�ff@պ^@�Q�@�ƨ@ӝ�@�+@��H@ҏ\@�=q@��@Ѳ-@�X@��@���@д9@�bN@��@���@�dZ@�"�@�@θR@ͩ�@�`B@�G�@�I�@˝�@�dZ@��H@���@�`B@���@ȓu@�b@Ǯ@�C�@�^5@Ł@�z�@�I�@�A�@�(�@�  @ÍP@��H@\@�n�@�M�@�-@��#@�`B@��@���@�r�@��@���@�M�@��T@��h@�X@���@��9@�A�@�dZ@���@�-@���@���@�1@��P@�;d@��@��R@���@��@�?}@��@��D@�Z@�9X@��@�~�@��@���@�hs@�7L@��`@�Q�@���@���@�C�@���@�=q@�hs@��`@�1'@���@�dZ@�o@��@���@�=q@���@���@��/@���@�(�@�|�@�C�@��H@��+@�-@��@��^@��7@��@��9@�Q�@�(�@��@�ƨ@�33@��!@�v�@�{@�@��7@�G�@��@��@���@��@� �@�|�@���@�^5@�5?@��@�p�@��@��D@���@��P@�t�@�33@���@���@�x�@�%@�Q�@��m@�t�@��@��@�ȴ@�~�@�=q@��@�`B@���@��j@�I�@��@��;@���@�t�@�33@��H@�~�@�V@�{@���@��h@�x�@�b@�`B@���@}?}@uV@k"�@_l�@W�@P �@H�u@?
=@6�R@.�y@*J@%p�@!�#@��@V@��@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	~�B	�B	�1B	�7B	�bB	��B	��B
�B
n�B
��B
��B��B�ZB�B��B��B�!B�!B�'B�wBȴB��B�B�HB�;B�`B  B��B��B��BBB��B�B�NB�BȴB�FB�'B��B��B��BƨB�qBĜB��B�#B�B��B�B�B�sB�mB�B�B�B	7B�B�B�ZB��BŢB�XB�B��B�DBw�BiyBM�B'�BB
�B
��B
�{B
�B
_;B
1'B	��B	ȴB	��B	u�B	P�B	8RB	&�B	�B�B��B��B�XB�?B�3BBŢBĜBĜBƨBȴBɺB��B��B��B��B��BB�!B��B��B��B��B�B�9B�9B�FB�XB��B�B	B	+B	1B��B�B�B�B�B	#�B	49B	<jB	s�B	��B	�B	�qB	��B	��B	�NB	�TB	�mB	�yB	�B	��B	��B	��B
B
DB
bB
bB
bB
{B
�B
�B
�B
{B
oB
oB
�B
"�B
#�B
$�B
 �B
%�B
,B
,B
+B
'�B
!�B
�B
�B
$�B
%�B
$�B
#�B
$�B
%�B
#�B
!�B
�B
�B
�B
oB
\B
bB
bB
hB
hB
hB
bB
\B
VB
\B
\B
PB
DB
	7B
	7B
	7B
PB
JB
	7B
+B
	7B

=B

=B

=B
1B
B
B
B
%B
B
B
B
  B	��B	��B	��B	��B
B
B
B
B
B
+B

=B
JB
JB
JB
DB
\B
oB
oB
hB
bB
bB
hB
hB
hB
hB
hB
hB
bB
bB
\B
VB
PB
DB
1B
+B
B
B
+B
JB
DB
DB
	7B
+B
B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�fB	�`B	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
JB
JB
VB
VB
\B
\B
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
oB
uB
oB
oB
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
+B
0!B
33B
;dB
C�B
J�B
L�B
Q�B
XB
^5B
dZB
hsB
l�B
o�B
r�B
w�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	~�B	�B	�1B	�=B	�hB	��B	�
B
�B
p�B
��B
��B��B�`B�B��B��B�-B�'B�-B�}BȴB��B�B�NB�;B�`BBB��BB
=B+B%B��B�`B�HB��B�XB�9BŢBȴB�
B��B��BǮB��B�5B��B��B�B�B�B�yB��B'�B�BuB�B�B�B�BɺB�jB�'B��B�hB{�Bq�BW
B2-BVB
�TB
��B
��B
�PB
l�B
B�B
B	�B	�!B	�+B	]/B	A�B	/B	0!B	B�/B��B�}B�^B�^BŢB��B��B��B��B��B��B��B��B�
B��B�BɺB�-B��B�B��B��B�B�LB�?B�XB�dB��B�sB	B	1B	PB	B��B�B�B�B	#�B	33B	6FB	n�B	��B	��B	�wB	B	ȴB	�NB	�TB	�sB	�B	�B	��B	��B	��B
B
PB
oB
oB
hB
�B
�B
�B
�B
�B
{B
oB
�B
$�B
$�B
&�B
!�B
%�B
-B
-B
-B
+B
$�B
�B
�B
%�B
'�B
%�B
$�B
%�B
&�B
$�B
#�B
 �B
�B
�B
{B
hB
hB
bB
hB
oB
oB
oB
hB
\B
\B
bB
bB
PB

=B
DB
	7B
VB
VB
DB
1B

=B
JB
JB
JB
DB
	7B
B
%B
+B
+B
B
B
B
  B
  B
  B	��B
B
B
B
B
%B
+B
DB
JB
PB
VB
JB
bB
uB
uB
{B
oB
hB
oB
hB
hB
hB
oB
oB
hB
hB
bB
\B
\B
PB
	7B
	7B
B
B
+B
PB
DB
JB

=B
	7B
B
B
  B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�fB	�sB	�yB	�yB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
  B
B
B
B
B
B
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
B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B

=B

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
\B
\B
\B
\B
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
+B
0!B
33B
;dB
C�B
K�B
M�B
Q�B
XB
^5B
dZB
hsB
l�B
o�B
r�B
w�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<�C�<e`B<u<�C�<�C�<D��<#�
<#�
<���<�1<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250292012011312502920120113125029  AO  ARGQ                                                                        20111205113425  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113425  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125029  IP                  G�O�G�O�G�O�                