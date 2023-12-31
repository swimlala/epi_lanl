CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:37Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               0A   AO  20111205113138  20190522121836  1901_5055_048                   2C  D   APEX                            2140                            040306                          846 @Ԡ�*�?�1   @Ԡ��À@/3�E����c(j~��#1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU�fDV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dvy�Dy` D��D�I�D�� D��fD���D�)�D��3D���D��fD��D�i�Dǳ3D��3D�3Dڌ�D๚D��fD��D�@ D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffB  BffB&ffB.ffB6ffB>ffBFffBNffBVffB^��BfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=�3C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�3Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDP` DP�fDQffDQ�fDRffDR�fDSffDS�fDT` DT�fDUl�DU�fDV` DV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo��DpffDp�fDqffDq�fDrffDr�fDsffDs�fDtl�Dt��DuffDu�fDv` DyFfD��D�<�D��3D���D�� D��D��fD�� D��D��D�\�DǦfD��fD�fDڀ D��D�ٚD�  D�33D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�O�A�O�A�Q�A�Q�A�XA�VA�VA�VA�ZA�^5A�dZA�l�A�hsA�r�A�x�AҍPA��#A�9XA�5?A��A���A��TAҝ�A�bA�~�A̾wA��A��A�7LAŰ!A��A��A�hsA���A�bA�VA�?}A��A�K�A��A��A�?}A��A�n�A��DA���A�VA�%A���A��A��mA��A�n�A�M�A�1'A�A��9A���A���A���A�v�A���A�JA���A�G�A�VA�$�A��A�
=A�%A�ĜA��uA�7LA�ffA���A�?}A�l�A�-A��;A���A�$�A�{A~I�Ax9XAsO�ApVAk�FAd��AaS�A`�HA_XA\��AZ��AUG�AQƨAO%AL�/AK�FAI��AG��AD��AB�uA@  A<ffA;C�A9�7A8M�A3%A0I�A/?}A-�A,��A-�A-A.VA.v�A/"�A2E�A2��A3VA2�A2��A2��A1/A/�hA1A1x�A0{A.�A-��A-VA,A�A+��A+
=A)��A'�^A&��A&A%`BA$~�A#`BA"�A"^5A!�A!;dA ��A jA��Ap�A�`A�A5?A�TAx�A^5A�7AȴA�!A�AQ�AƨA��AƨA�-A�AG�AĜA�9A��AA�A  AĜA�#A��A�^AƨA�AdZA�/A��AȴA9XA1AƨA�-A�7AK�AG�A7LAVA�\A?}A~�A5?A�Ax�A�A�A�A�+A��AffA�#AK�A
��A	�A	�A	��A	�A�AG�A�\A��A�^A�A�PA��A�RA��A��Ap�A��Av�AbNA��A��AhsA�A �+A M�A -@�ƨ@�J@�V@� �@��H@�^5@�G�@��F@�\)@���@�x�@���@�A�@�P@�M�@�`B@�X@�X@��@@��@��@���@�h@�7L@�j@띲@�K�@��T@�`B@��`@��m@�~�@噚@�Ĝ@䛦@�j@�1@�|�@���@�R@�\@��@�p�@�r�@��;@�|�@�"�@�@��y@�^5@ݑh@�7L@��@ܓu@�9X@�ƨ@��@���@���@�`B@�%@�bN@׮@ׅ@�t�@�C�@��y@�5?@�x�@Չ7@�p�@��`@�I�@��;@ӥ�@�S�@�+@���@�n�@�@��T@��T@Ѳ-@��@Ϯ@�o@Χ�@�J@�%@�bN@�b@˕�@��@���@�$�@�G�@��@ț�@�A�@���@���@Ǖ�@�"�@�v�@�$�@ŉ7@�O�@�hs@�x�@��@ă@�9X@Å@�~�@��@�`B@��D@�j@�Ĝ@��9@�r�@�\)@�"�@�
=@�ȴ@�ff@��@�@���@�x�@��@�Z@�1'@�(�@� �@���@�l�@�C�@�v�@�=q@���@��@�/@��`@�bN@� �@��
@�t�@�C�@���@�ff@�=q@�-@��@���@���@�/@�Ĝ@���@��D@�Q�@��m@��w@�t�@�@��R@�ff@�E�@�-@���@�7L@��@���@�K�@��H@�5?@��^@�`B@���@��u@�(�@��m@��w@��F@�t�@�o@���@�M�@���@�O�@���@�9X@��@��m@�S�@��@�n�@��T@�@��@�G�@��@��@��9@�z�@� �@��@��m@��
@�l�@�ȴ@�E�@���@�/@���@�r�@��@��F@�t�@�;d@�o@��H@�~�@��@��7@�hs@�O�@�G�@�%@��`@���@�Z@��@��F@�K�@���@�~�@��T@��7@�hs@��@���@�bN@��@��F@�l�@�K�@��@�~�@�M�@�{@�J@���@�x�@�7L@�V@���@�I�@� �@��y@�hs@�ȴ@���@zn�@r~�@i�@a7L@Z�\@Q�@H�u@B�\@;��@2�@*n�@$�@!7L@j@5?@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�O�A�O�A�Q�A�Q�A�XA�VA�VA�VA�ZA�^5A�dZA�l�A�hsA�r�A�x�AҍPA��#A�9XA�5?A��A���A��TAҝ�A�bA�~�A̾wA��A��A�7LAŰ!A��A��A�hsA���A�bA�VA�?}A��A�K�A��A��A�?}A��A�n�A��DA���A�VA�%A���A��A��mA��A�n�A�M�A�1'A�A��9A���A���A���A�v�A���A�JA���A�G�A�VA�$�A��A�
=A�%A�ĜA��uA�7LA�ffA���A�?}A�l�A�-A��;A���A�$�A�{A~I�Ax9XAsO�ApVAk�FAd��AaS�A`�HA_XA\��AZ��AUG�AQƨAO%AL�/AK�FAI��AG��AD��AB�uA@  A<ffA;C�A9�7A8M�A3%A0I�A/?}A-�A,��A-�A-A.VA.v�A/"�A2E�A2��A3VA2�A2��A2��A1/A/�hA1A1x�A0{A.�A-��A-VA,A�A+��A+
=A)��A'�^A&��A&A%`BA$~�A#`BA"�A"^5A!�A!;dA ��A jA��Ap�A�`A�A5?A�TAx�A^5A�7AȴA�!A�AQ�AƨA��AƨA�-A�AG�AĜA�9A��AA�A  AĜA�#A��A�^AƨA�AdZA�/A��AȴA9XA1AƨA�-A�7AK�AG�A7LAVA�\A?}A~�A5?A�Ax�A�A�A�A�+A��AffA�#AK�A
��A	�A	�A	��A	�A�AG�A�\A��A�^A�A�PA��A�RA��A��Ap�A��Av�AbNA��A��AhsA�A �+A M�A -@�ƨ@�J@�V@� �@��H@�^5@�G�@��F@�\)@���@�x�@���@�A�@�P@�M�@�`B@�X@�X@��@@��@��@���@�h@�7L@�j@띲@�K�@��T@�`B@��`@��m@�~�@噚@�Ĝ@䛦@�j@�1@�|�@���@�R@�\@��@�p�@�r�@��;@�|�@�"�@�@��y@�^5@ݑh@�7L@��@ܓu@�9X@�ƨ@��@���@���@�`B@�%@�bN@׮@ׅ@�t�@�C�@��y@�5?@�x�@Չ7@�p�@��`@�I�@��;@ӥ�@�S�@�+@���@�n�@�@��T@��T@Ѳ-@��@Ϯ@�o@Χ�@�J@�%@�bN@�b@˕�@��@���@�$�@�G�@��@ț�@�A�@���@���@Ǖ�@�"�@�v�@�$�@ŉ7@�O�@�hs@�x�@��@ă@�9X@Å@�~�@��@�`B@��D@�j@�Ĝ@��9@�r�@�\)@�"�@�
=@�ȴ@�ff@��@�@���@�x�@��@�Z@�1'@�(�@� �@���@�l�@�C�@�v�@�=q@���@��@�/@��`@�bN@� �@��
@�t�@�C�@���@�ff@�=q@�-@��@���@���@�/@�Ĝ@���@��D@�Q�@��m@��w@�t�@�@��R@�ff@�E�@�-@���@�7L@��@���@�K�@��H@�5?@��^@�`B@���@��u@�(�@��m@��w@��F@�t�@�o@���@�M�@���@�O�@���@�9X@��@��m@�S�@��@�n�@��T@�@��@�G�@��@��@��9@�z�@� �@��@��m@��
@�l�@�ȴ@�E�@���@�/@���@�r�@��@��F@�t�@�;d@�o@��H@�~�@��@��7@�hs@�O�@�G�@�%@��`@���@�Z@��@��F@�K�@���@�~�@��T@��7@�hs@��@���@�bN@��@��F@�l�@�K�@��@�~�@�M�@�{@�J@���@�x�@�7L@�V@���@�I�@� �@��y@�hs@�ȴ@���@zn�@r~�@i�@a7L@Z�\@Q�@H�u@B�\@;��@2�@*n�@$�@!7L@j@5?@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
%B
1B
%B

=B
JB
"�B
w�B
�B?}B?}B>wB<jB6FB'�B
��B
��B
��B
�DB
�B
~�B
v�B
v�B
��B
�B
�RB
��B
�B
�;B
��BL�BS�BXBS�BI�BL�BZBp�B�uB�jBƨBoBO�BZBR�BM�B^5BJ�B�B�BJB-B7LB5?B=qBK�BN�BG�BB�B<jB0!B�B��B�B�jB��B�7B9XB
��B
��B
@�B
B	�#B	ÖB	�9B	��B	}�B	YB	+B	 �B	 �B	{B	1B��B��B��B	%B	%B	+B		7B	�B	�B	�B	1B��B��B�B�B�
B��B��BÖBÖB�NB	  B	uB	�B	,B	�{B	�B	ɺB	��B	��B	��B	�jB	�jB	�ZB	��B
  B	��B	��B
B

=B
DB
JB
%B	�B	�B	�B	�B	�B	�B	�mB	�mB	�yB	�B	�B	�B	��B
B
1B
	7B

=B
DB
DB
DB

=B
PB
\B
bB
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
(�B
+B
%�B
"�B
%�B
,B
2-B
33B
2-B
0!B
33B
7LB
49B
2-B
1'B
2-B
33B
33B
33B
33B
33B
1'B
-B
)�B
)�B
+B
&�B
$�B
$�B
'�B
(�B
!�B
�B
�B
oB
\B
PB
DB
DB

=B
B
B
B
  B
B
%B
+B
B
B
	7B
B
B	��B	��B	��B	��B	��B
B
  B	��B	��B
  B
B
  B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B
  B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
B
  B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B
1B
1B

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
JB
JB
VB
VB
VB
\B
\B
bB
\B
\B
\B
VB
\B
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
+B
/B
5?B
7LB
=qB
@�B
G�B
M�B
S�B
XB
]/B
dZB
jB
m�B
p�B
s�B
y�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
%B
1B
%B

=B
JB
!�B
v�B
�B@�B@�B?}B=qB9XB33BB
�B
��B
�VB
�%B
�B
�B
|�B
��B
�!B
�jB
��B
�/B
�HB
��BM�BVB[#BYBM�BP�BaHBv�B��B�wB��BoBQ�B]/BVBQ�BcTBQ�B"�B�BPB0!B;dB7LB?}BL�BS�BJ�BK�BB�B7LB+B	7B�TBÖB��B��BI�B
�B
��B
K�B

=B	�;B	��B	�}B	��B	�DB	l�B	5?B	"�B	&�B	�B	hB	\B	B	B	VB	
=B	VB	VB	'�B	$�B	�B	oB��B��B��B��B�5B��B��BƨBÖB�HB��B	uB	�B	%�B	�{B	�B	��B	��B	��B	��B	��B	�XB	�TB
B
B
B	��B
B
JB
PB
bB
PB	��B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	��B	��B
B
	7B

=B
DB
PB
\B
VB
PB
VB
\B
hB
oB
hB
�B
�B
�B
 �B
�B
�B
�B
�B
)�B
.B
(�B
#�B
%�B
,B
33B
49B
49B
0!B
33B
9XB
5?B
33B
1'B
33B
49B
33B
33B
49B
5?B
5?B
/B
+B
+B
-B
(�B
%�B
%�B
'�B
,B
%�B
�B
�B
{B
hB
VB
JB
PB
VB
+B
B
B
B
B
+B
	7B
B
B
JB
B
B
B	��B
  B	��B
  B
B
B
  B	��B
B
B
B
  B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B
B
B	��B
  B
  B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B

=B
	7B
	7B
	7B

=B
1B
	7B
DB
JB
JB
JB
JB
JB
JB
JB
JB
DB
DB
JB
PB
PB
VB
PB
PB
\B
\B
\B
bB
bB
hB
\B
bB
bB
\B
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
+B
0!B
5?B
7LB
=qB
@�B
G�B
N�B
S�B
XB
^5B
dZB
jB
m�B
q�B
t�B
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
<49X<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<#�
<#�
<#�
<D��<�o<#�
<D��<49X<#�
<#�
<49X<49X<#�
<D��<�t�<#�
<#�
<#�
<#�
<#�
<�t�<T��<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250242012011312502420120113125024  AO  ARGQ                                                                        20111205113138  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113138  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125024  IP                  G�O�G�O�G�O�                