CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               2A   AO  20111130144015  20190522121829  1728_5048_050                   2C  D   APEX                            2142                            040306                          846 @ԧ�1   @ԧ���@5�z�G��c�E���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy��D��D�,�D�i�D�ɚD�fD�&fD��D�� D��D�)�D�� DǬ�D�� D�0 Dڀ Dਗ਼D��D�  D�Vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33@�33A��A8  AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C� C��C��C!��C#��C%��C'��C)��C+��C-��C/� C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fD` D�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9` D9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGl�DG�fDHffDH�fDIffDI��DJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQl�DQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`l�D`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh� Di` Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDys3D�  D�  D�\�D���D���D��D� D��3D���D��D��3DǠ D��3D�#3D�s3D���D���D�3D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A���A���A���A���A��A��A��A��;A��/A��HA��;A���AƟ�A�jA� �A�^5AĲ-A� �A���AÑhA�z�A�`BA�M�A�?}A��A�A���A���A��/A¬A�^5A��A���A�9XA�n�A�A�I�A�z�A�E�A��wA�JA���A�oA��A��A��-A���A��hA��A��#A��A���A�7LA��/A�^5A���A��hA���A��A�1'A��uA��/A���A�K�A�ffA��A��;A�\)A��TA��A�\)A���A��+A�A�9XA���A�C�A��!A�I�A��PA�C�A��#A�O�A��A�Q�A�bNA�bA��A�"�A��A��A�&�A���A�C�A�O�A�Q�A��A~-A|�\A{l�A{oAy�TAv(�AsS�Aq��Ao�7An�Al�Ak�PAjffAi�PAh(�Af�Ad�Ac��Ab�HAa��AaK�A`9XA\~�A[�7AZ�\AYXAX��AXM�AW�AU��AS�AP�`APZAO�TAN��ALĜAK/AJbAI;dAH �AFv�ADA�ABQ�A@E�A>�jA<�uA:�RA8�yA7��A69XA5�hA4{A3�7A2z�A1XA0A�A/33A.�A.I�A-��A-��A,��A+ƨA+dZA+?}A+%A*��A*  A)�wA)�A(n�A'��A't�A&�jA&1A%%A$ffA$A�A#�
A#"�A"�uA!�A �+A�A�+A��A
=AE�A�wAO�A��AA�A�wA��A�A+AffA�-A%AM�A
=A�AjAĜA�A�uAA�A�A33A	A	VA�\AA�PAO�A+A�/A�A��A�#AȴA �H@�dZ@��@�b@��@�z�@���@�@�ff@�  @�
=@���@�@�
=@�M�@���@�r�@�+@�V@�@�-@�7@��@�@ڗ�@�hs@�%@��/@أ�@��@�K�@�=q@�b@�S�@�33@��@�@���@���@��y@��@җ�@Ѻ^@���@ёh@�V@мj@�"�@́@�r�@�33@�n�@�@���@��
@��H@���@���@�Q�@��@Å@�+@�@��7@�A�@���@��@��+@��`@��y@��D@���@��-@��h@�^5@��!@�V@���@���@�`B@�V@�"�@�n�@��`@���@�X@�9X@�@�E�@�"�@���@��F@�S�@��@�/@�=q@���@�\)@���@�@��@�C�@�$�@�?}@��u@�r�@��P@��@�=q@�/@��9@�|�@��@�=q@�{@�$�@�ff@��H@���@���@�t�@��F@�b@���@�33@�V@�C�@��\@��@�%@�O�@�7L@��j@��u@�M�@�bN@�5?@��!@�\)@��m@�Z@���@���@�bN@�r�@���@�Ĝ@���@��u@�z�@�(�@��m@��P@�S�@�"�@��!@�n�@�=q@��@�$�@�@�`B@�%@��/@��9@���@�r�@��@��@�;d@���@��R@��+@�5?@���@��@���@��-@���@��@�?}@��j@��@��u@��@��D@��/@��@�j@� �@���@�o@��!@�ff@�5?@�J@��7@��@���@��@��@��9@���@��D@�Q�@�b@��P@�;d@�
=@��@��R@�V@���@�@�X@���@��@�9X@���@��
@�ƨ@��w@��P@�C�@�@��@���@���@���@��\@�ff@��@���@���@���@��7@��@�x�@�hs@�X@�/@��@�z�@�A�@��;@��F@�\)@�+@�"�@�"�@��@���@��H@��!@��+@�v�@�^5@�$�@���@��@���@���@�x�@�G�@��@�%@���@��@��/@��@�Q�@�b@�|�@+@t�@k33@d1@Z�\@S"�@L�D@D�@=��@7�@1hs@+o@&ff@"��@��@�@�h@�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��A���A���A���A���A��A��A��A��;A��/A��HA��;A���AƟ�A�jA� �A�^5AĲ-A� �A���AÑhA�z�A�`BA�M�A�?}A��A�A���A���A��/A¬A�^5A��A���A�9XA�n�A�A�I�A�z�A�E�A��wA�JA���A�oA��A��A��-A���A��hA��A��#A��A���A�7LA��/A�^5A���A��hA���A��A�1'A��uA��/A���A�K�A�ffA��A��;A�\)A��TA��A�\)A���A��+A�A�9XA���A�C�A��!A�I�A��PA�C�A��#A�O�A��A�Q�A�bNA�bA��A�"�A��A��A�&�A���A�C�A�O�A�Q�A��A~-A|�\A{l�A{oAy�TAv(�AsS�Aq��Ao�7An�Al�Ak�PAjffAi�PAh(�Af�Ad�Ac��Ab�HAa��AaK�A`9XA\~�A[�7AZ�\AYXAX��AXM�AW�AU��AS�AP�`APZAO�TAN��ALĜAK/AJbAI;dAH �AFv�ADA�ABQ�A@E�A>�jA<�uA:�RA8�yA7��A69XA5�hA4{A3�7A2z�A1XA0A�A/33A.�A.I�A-��A-��A,��A+ƨA+dZA+?}A+%A*��A*  A)�wA)�A(n�A'��A't�A&�jA&1A%%A$ffA$A�A#�
A#"�A"�uA!�A �+A�A�+A��A
=AE�A�wAO�A��AA�A�wA��A�A+AffA�-A%AM�A
=A�AjAĜA�A�uAA�A�A33A	A	VA�\AA�PAO�A+A�/A�A��A�#AȴA �H@�dZ@��@�b@��@�z�@���@�@�ff@�  @�
=@���@�@�
=@�M�@���@�r�@�+@�V@�@�-@�7@��@�@ڗ�@�hs@�%@��/@أ�@��@�K�@�=q@�b@�S�@�33@��@�@���@���@��y@��@җ�@Ѻ^@���@ёh@�V@мj@�"�@́@�r�@�33@�n�@�@���@��
@��H@���@���@�Q�@��@Å@�+@�@��7@�A�@���@��@��+@��`@��y@��D@���@��-@��h@�^5@��!@�V@���@���@�`B@�V@�"�@�n�@��`@���@�X@�9X@�@�E�@�"�@���@��F@�S�@��@�/@�=q@���@�\)@���@�@��@�C�@�$�@�?}@��u@�r�@��P@��@�=q@�/@��9@�|�@��@�=q@�{@�$�@�ff@��H@���@���@�t�@��F@�b@���@�33@�V@�C�@��\@��@�%@�O�@�7L@��j@��u@�M�@�bN@�5?@��!@�\)@��m@�Z@���@���@�bN@�r�@���@�Ĝ@���@��u@�z�@�(�@��m@��P@�S�@�"�@��!@�n�@�=q@��@�$�@�@�`B@�%@��/@��9@���@�r�@��@��@�;d@���@��R@��+@�5?@���@��@���@��-@���@��@�?}@��j@��@��u@��@��D@��/@��@�j@� �@���@�o@��!@�ff@�5?@�J@��7@��@���@��@��@��9@���@��D@�Q�@�b@��P@�;d@�
=@��@��R@�V@���@�@�X@���@��@�9X@���@��
@�ƨ@��w@��P@�C�@�@��@���@���@���@��\@�ff@��@���@���@���@��7@��@�x�@�hs@�X@�/@��@�z�@�A�@��;@��F@�\)@�+@�"�@�"�@��@���@��H@��!@��+@�v�@�^5@�$�@���@��@���@���@�x�@�G�@��@�%@���@��@��/@��@�Q�@�b@�|�@+@t�@k33@d1@Z�\@S"�@L�D@D�@=��@7�@1hs@+o@&ff@"��@��@�@�h@�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B� B� B~�B~�B~�B~�B~�B}�B}�B|�B~�B�B�oB��B�B�-B�9B�?B�LB�XB�wB��B��BBƨBȴBǮB�dB��B��B�B�LB�3B�XB�LB�LB�B��B�{B�BiyBA�B_;B}�B�B�B|�Bw�B~�B}�B{�Bt�Bp�BjBbNBS�BYBT�BO�BD�BM�B?}B<jB6FB&�B!�B�BVBB�B�NB�)B��B��B�dB�3B��By�Bn�BgmBO�B;dB�BbBDBB
�sB
��B
�-B
�hB
p�B
[#B
G�B
9XB
,B
$�B
�B
uB
VB
+B	�B	�`B	�B	ɺB	��B	ƨB	ÖB	�XB	��B	��B	�{B	�bB	�DB	�%B	� B	w�B	ffB	_;B	ZB	S�B	R�B	Q�B	I�B	D�B	5?B	-B	)�B	%�B	 �B	�B	bB	1B	B��B�B�mB�;B��B��BŢB�^B�FB��B�-B��B��B��B��B�?B�B��B��B��B��B��B��B��B��B�B�B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�VB�=B�JB�7B�1B�B�B~�B}�Bw�Bw�Bm�BiyBiyBhsBgmBjBe`BaHBaHB_;BcTB^5B]/B]/B\)B\)B[#B[#B\)BYBZBYBYBZBYBT�BYB\)B^5BA�BA�BB�B=qB49B:^B5?B.B/B49B5?B1'B?}BC�BA�BA�B@�B<jB?}B=qB<jB;dB6FB0!B0!B0!B0!B0!B1'B2-B49B=qBE�BG�BM�BT�BW
B[#B^5BaHBffBhsBo�Bq�Bt�Bw�Bz�B|�B~�B� B�B�B�=B�1B�7B�JB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�1B�%B�%B�JB��B��B�B�-B��B��B��B��B��B�3B��BŢB�}B�qBȴB��B�5B�ZB�`B�NB�TB�B��B��B	B	B	B	B	1B	B	B	B	
=B	PB	�B	oB	uB	�B	uB	�B	�B	�B	�B	�B	)�B	2-B	1'B	6FB	9XB	D�B	G�B	?}B	F�B	H�B	B�B	C�B	H�B	L�B	M�B	N�B	[#B	ZB	N�B	R�B	YB	_;B	bNB	e`B	k�B	l�B	o�B	q�B	u�B	v�B	v�B	x�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�=B	�=B	�JB	�JB	�JB	�PB	�PB	�VB	�VB	�VB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�FB	�LB	�RB	�XB	�jB	�jB	�wB	��B	B	ĜB	ĜB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�HB	�NB	�NB	�TB	�ZB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B
B
DB
�B
#�B
)�B
1'B
7LB
>wB
E�B
J�B
P�B
VB
\)B
_;B
ffB
hsB
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B� B� B~�B~�B~�B~�B~�B}�B~�B}�B�B�%B��B��B�B�3B�?B�FB�LB�XB�}BB��BBǮBɺB��BĜB�9B�B�-B�XB�FB�jB��B�wB�LB��B��B�1Bs�BC�B_;B� B�B�7B~�B{�B�B� B~�Bv�Bs�Bs�BhsBXB\)BXBS�BF�BQ�BA�B=qB=qB(�B#�B#�BoBDB��B�`B�;B��B��B�wB�qB��B|�Bq�Bn�BS�BE�B�BuBVB1B
�B
�)B
�jB
��B
v�B
bNB
O�B
?}B
1'B
(�B
�B
�B
�B
\B	��B	�B	�)B	��B	��B	ɺB	ŢB	�qB	��B	��B	��B	�uB	�VB	�1B	�B	�B	iyB	bNB	^5B	VB	T�B	VB	M�B	L�B	;dB	/B	,B	)�B	&�B	�B	{B	DB	1B	B��B�B�`B�B��B��B�}B�XB�B�9B��B��B��B�B�RB�B��B��B��B��B��B��B�B��B�B�B�-B�B�B�B�'B�B�B�B��B��B��B��B��B��B��B��B��B�bB�PB�\B�JB�=B�+B�+B�B�B{�B{�Bp�Bl�Bl�Bk�BjBm�BjBbNBcTBe`BiyB`BB_;B_;B_;BaHB^5B]/B^5B[#B[#BZB[#B]/B^5BYB]/BaHBbNBC�BD�BF�B>wB9XB<jB7LB1'B1'B7LB7LB2-B@�BD�BC�BC�BB�B@�BB�B>wB?}B?}B;dB2-B1'B1'B1'B2-B33B49B8RB?}BF�BH�BM�BT�BW
B[#B^5BaHBhsBhsBp�Br�Bu�Bz�B}�B~�B�B�B�B�B�JB�=B�DB�JB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B�JB�1B�%B�DB��B��B�!B�?B��B��B��B��B��B�-B��BǮBÖB�qBǮB��B�5B�`B�sB�TB�NB�B��B	  B	B	%B	B	B	
=B	%B	B	B	DB	VB	�B	uB	�B	�B	{B	�B	�B	�B	�B	�B	+B	33B	1'B	6FB	9XB	E�B	H�B	>wB	G�B	J�B	C�B	C�B	H�B	M�B	M�B	L�B	^5B	^5B	N�B	Q�B	XB	_;B	bNB	e`B	l�B	l�B	o�B	q�B	v�B	v�B	v�B	y�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�7B	�=B	�DB	�DB	�DB	�=B	�DB	�DB	�DB	�DB	�JB	�=B	�DB	�PB	�PB	�JB	�VB	�PB	�VB	�VB	�\B	�\B	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�FB	�LB	�LB	�XB	�^B	�qB	�qB	�}B	��B	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�NB	�TB	�TB	�ZB	�ZB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B
B
DB
�B
#�B
+B
1'B
7LB
>wB
E�B
J�B
P�B
W
B
\)B
`BB
ffB
hsB
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452102012011014521020120110145210  AO  ARGQ                                                                        20111130144015  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144015  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145210  IP                  G�O�G�O�G�O�                