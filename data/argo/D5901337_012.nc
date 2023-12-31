CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:26Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112504  20190522121836  1901_5055_012                   2C  D   APEX                            2140                            040306                          846 @�G=!��1   @�G=��?�@.Qhr� ��cd�\)1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C�fC�fC  C  C   C"  C$�C%�fC'�fC)�fC+�fC-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CM�fCP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy�fD���D�@ D�|�D�� D�  D�9�D�� D��fD�fD�33D�� DǶfD��3D�  Dڀ D��3D�� D��D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A9��AY��Ay��A���A���A���A���A���Aݙ�A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBN��BVffB^  BfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�ffB�ffB���B�  B�33B�  B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33C��C��C��C��C	��C��C��C��C��C��C��C� C� C��C��C��C!��C#�3C%� C'� C)� C+� C-� C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE�3CG�3CI��CK��CM� CO��CQ��CS��CU��CW� CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo��Dpl�Dp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwY�Dyl�D���D�33D�p D��3D��3D�,�D�s3D���D���D�&fD�s3Dǩ�D��fD�3D�s3D�fD��3D��D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A܁A܁A�p�A�hsA�dZA�bNA�`BA�bNA�dZA�bNA�\)A�O�A�K�A�C�A�?}A�7LA�(�A��A���A۸RA�I�Aڝ�A�7LA���A��A�ƨA�$�A���A��A�jA�
=A���A�x�A�C�A��A�E�A�
=A̗�A���A���A��wA���A��A���A�7LA���A���A��uA���A�p�A��A��PA�5?A��A�M�A��A�t�A�  A��A���A��\A�+A�A�r�A�  A��A�A�O�A��7A��A�|�A��PA���A��
A��A�t�A���A�ZA���A���A�t�A|1'Au�PAq��An��Al�Ak�#AkK�Aj�jAi�wAh$�Aet�A`��A`I�A]ƨA[;dAZZAU�AQ�TAL��AH(�AE33ADr�AC�TAA�
A?��A>�jA>��A?��A@�AAC�AAG�AAA@  A?��A>��A=�A;XA933A7�;A6E�A2�!A.��A-�mA,z�A*v�A)l�A(��A'��A'�wA'��A'�A&��A%�FA$=qA#�-A"�A!;dA {A9XA��A�mAz�A�7AG�AhsAS�AhsA�hA��A��At�A��A�/A��AVA��A�+A�A�7A�A�!AA�At�A��AoAVA�!A��AA��A��AS�AoA�A1A?}A
��A
 �A	K�A�DA�AVAjA�yA��A�A�yA��A/A j@��y@��-@���@��m@���@�-@�v�@��@�b@���@�A�@��@�
=@���@�1@�X@�@柾@�E�@��@���@�@�;d@�@�X@��`@��u@��m@�\)@ޏ\@ݩ�@�?}@ܴ9@�j@��m@��@٩�@؋D@���@��@�7L@ԣ�@�1'@�C�@�ȴ@�=q@��@ѡ�@��@Ѓ@��m@�K�@�~�@�M�@�$�@��#@�`B@���@̬@�b@ˍP@�l�@��@ʰ!@�ff@��@�x�@���@�A�@�(�@��;@Ǖ�@�\)@�C�@�@��@Ɵ�@�^5@�$�@��#@�hs@�V@Ĵ9@�A�@�(�@��@�1@Õ�@��H@°!@�@�5?@��#@���@��@�r�@�b@���@�|�@�o@�ȴ@�v�@�@��#@�/@��D@�bN@�(�@��@���@�33@��@�ȴ@���@��/@�j@�9X@���@�33@���@��\@�ff@�=q@�$�@�{@��#@��@���@�Z@� �@��@�\)@�@�^5@��7@�&�@���@��j@�r�@���@�"�@�o@�ȴ@�-@��-@�p�@�7L@��9@��u@�bN@�1@��P@�C�@��@��@��!@�v�@���@�v�@���@�hs@��@�  @��@���@�|�@�l�@�|�@�t�@�33@��@���@�v�@�5?@��T@�`B@�/@�/@���@��u@�j@��m@�ƨ@��@�5?@��@��-@��h@�&�@��@���@���@��u@��m@���@�t�@�\)@��@���@�J@���@�`B@��@��9@��@�j@�j@�Z@�1'@��;@��@�"�@�v�@�{@��^@��-@�@�O�@�%@�Ĝ@�9X@��
@��w@��@�\)@��@���@�V@��#@��@��D@��D@��D@��@�b@���@�C�@�33@�+@��@�@��y@��\@�~�@�V@�M�@�=q@��@���@��@���@��9@�z�@�(�@��@�  @��;@��@��P@�\)@�o@��@��\@�$�@���@���@���@�&�@��@��j@�r�@�(�@��;@�S�@��@���@��\@�n�@�=q@�@���@�X@�`B@�&�@���@���@��D@�r�@�j@��@���@��@�33@���@���@�ff@�J@���@��h@�7L@��@�V@�bN@��^@z=q@o��@e��@\z�@TZ@K@A��@9�@1��@+�
@&{@"^5@I�@�@=q@�@S�@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A܁A܁A�p�A�hsA�dZA�bNA�`BA�bNA�dZA�bNA�\)A�O�A�K�A�C�A�?}A�7LA�(�A��A���A۸RA�I�Aڝ�A�7LA���A��A�ƨA�$�A���A��A�jA�
=A���A�x�A�C�A��A�E�A�
=A̗�A���A���A��wA���A��A���A�7LA���A���A��uA���A�p�A��A��PA�5?A��A�M�A��A�t�A�  A��A���A��\A�+A�A�r�A�  A��A�A�O�A��7A��A�|�A��PA���A��
A��A�t�A���A�ZA���A���A�t�A|1'Au�PAq��An��Al�Ak�#AkK�Aj�jAi�wAh$�Aet�A`��A`I�A]ƨA[;dAZZAU�AQ�TAL��AH(�AE33ADr�AC�TAA�
A?��A>�jA>��A?��A@�AAC�AAG�AAA@  A?��A>��A=�A;XA933A7�;A6E�A2�!A.��A-�mA,z�A*v�A)l�A(��A'��A'�wA'��A'�A&��A%�FA$=qA#�-A"�A!;dA {A9XA��A�mAz�A�7AG�AhsAS�AhsA�hA��A��At�A��A�/A��AVA��A�+A�A�7A�A�!AA�At�A��AoAVA�!A��AA��A��AS�AoA�A1A?}A
��A
 �A	K�A�DA�AVAjA�yA��A�A�yA��A/A j@��y@��-@���@��m@���@�-@�v�@��@�b@���@�A�@��@�
=@���@�1@�X@�@柾@�E�@��@���@�@�;d@�@�X@��`@��u@��m@�\)@ޏ\@ݩ�@�?}@ܴ9@�j@��m@��@٩�@؋D@���@��@�7L@ԣ�@�1'@�C�@�ȴ@�=q@��@ѡ�@��@Ѓ@��m@�K�@�~�@�M�@�$�@��#@�`B@���@̬@�b@ˍP@�l�@��@ʰ!@�ff@��@�x�@���@�A�@�(�@��;@Ǖ�@�\)@�C�@�@��@Ɵ�@�^5@�$�@��#@�hs@�V@Ĵ9@�A�@�(�@��@�1@Õ�@��H@°!@�@�5?@��#@���@��@�r�@�b@���@�|�@�o@�ȴ@�v�@�@��#@�/@��D@�bN@�(�@��@���@�33@��@�ȴ@���@��/@�j@�9X@���@�33@���@��\@�ff@�=q@�$�@�{@��#@��@���@�Z@� �@��@�\)@�@�^5@��7@�&�@���@��j@�r�@���@�"�@�o@�ȴ@�-@��-@�p�@�7L@��9@��u@�bN@�1@��P@�C�@��@��@��!@�v�@���@�v�@���@�hs@��@�  @��@���@�|�@�l�@�|�@�t�@�33@��@���@�v�@�5?@��T@�`B@�/@�/@���@��u@�j@��m@�ƨ@��@�5?@��@��-@��h@�&�@��@���@���@��u@��m@���@�t�@�\)@��@���@�J@���@�`B@��@��9@��@�j@�j@�Z@�1'@��;@��@�"�@�v�@�{@��^@��-@�@�O�@�%@�Ĝ@�9X@��
@��w@��@�\)@��@���@�V@��#@��@��D@��D@��D@��@�b@���@�C�@�33@�+@��@�@��y@��\@�~�@�V@�M�@�=q@��@���@��@���@��9@�z�@�(�@��@�  @��;@��@��P@�\)@�o@��@��\@�$�@���@���@���@�&�@��@��j@�r�@�(�@��;@�S�@��@���@��\@�n�@�=q@�@���@�X@�`B@�&�@���@���@��D@�r�@�j@��@���@��@�33@���@���@�ff@�J@���@��h@�7L@��@�V@�bN@��^@z=q@o��@e��@\z�@TZ@K@A��@9�@1��@+�
@&{@"^5@I�@�@=q@�@S�@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�RB	�RB	�LB	�RB	�XB	�XB	�^B	�dB	�^B	�dB	�dB	�jB	�jB	�qB	�wB	�wB	��B	��B	ÖB	ŢB	��B	��B	�ZB	�B	��B
PB
�+B
�sB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�}B
��B
��B
��B
�B
�'B
B
�B
��BhB�B'�B5?B=qB>wBVB`BB^5BW
BO�BD�B"�B\BDBJBPB
��B
�BB
��B
�}B
�9B
�B
aHB
Q�B
8RB
�B
oB

=B	��B	�B	�5B	�!B	n�B	%B�B�B�B�B�B�B�B�mB�NB�
B��B��BÖB�}B�dB�!B��B�oB�1B�JB�bB�uB��B�?B��B�B	JB	D�B	W
B	n�B	z�B	�B	�1B	�{B	��B	��B	��B	��B	�bB	s�B	bNB	iyB	cTB	]/B	\)B	_;B	dZB	gmB	hsB	jB	iyB	gmB	`BB	[#B	W
B	O�B	G�B	B�B	B�B	<jB	2-B	2-B	49B	<jB	F�B	T�B	hsB	q�B	w�B	x�B	{�B	�B	�B	�B	�B	�%B	�=B	��B	�oB	�hB	�bB	�bB	�oB	�{B	��B	��B	�uB	�hB	�\B	�VB	�VB	�PB	�JB	�1B	�%B	�7B	�%B	�B	�DB	�1B	�B	y�B	s�B	~�B	� B	}�B	r�B	ffB	e`B	cTB	cTB	ffB	jB	k�B	l�B	n�B	n�B	l�B	dZB	\)B	hsB	dZB	_;B	aHB	_;B	^5B	ffB	k�B	k�B	m�B	q�B	x�B	z�B	}�B	�B	�B	�B	�1B	�JB	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�9B	�3B	�9B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�qB	�jB	�jB	�qB	�qB	�}B	�}B	��B	��B	��B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�TB	�TB	�ZB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B
	7B
1B
1B
1B
1B

=B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB

=B
JB
VB
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
.B
2-B
6FB
;dB
C�B
J�B
Q�B
XB
]/B
bNB
ffB
k�B
m�B
q�B
w�B
}�B
� B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�RB	�RB	�LB	�RB	�XB	�XB	�^B	�dB	�^B	�dB	�dB	�jB	�jB	�qB	�wB	�wB	��B	��B	ĜB	ƨB	��B	�B	�fB	�B	��B
�B
�oB
�B
��B
��B  B
��B
��B
��B
��B
��B
�B
�B
�/B
��B
�RB
��B
�B
�^B
�?B
ĜB
�B
��BhB�B,B=qBG�BC�BZBbNBaHBZBW
BM�B)�B�BbB\BuB  B
�ZB
�
B
ɺB
ƨB
�DB
e`B
XB
?}B
�B
�B
hB	��B	�B	�sB	�XB	� B	\B��B�B�B�B�B�B�B�B�B�`B��B��B��B��BB�?B��B��B�JB�VB�oB��B��B�RB��B��B		7B	C�B	W
B	o�B	~�B	�B	�JB	��B	��B	�!B	��B	��B	��B	|�B	e`B	m�B	iyB	`BB	^5B	bNB	e`B	hsB	jB	l�B	l�B	l�B	bNB	`BB	ZB	S�B	L�B	F�B	E�B	@�B	5?B	33B	49B	<jB	F�B	T�B	k�B	t�B	x�B	z�B	|�B	�B	�B	�1B	�B	�1B	�JB	��B	�uB	�uB	�uB	�oB	�oB	��B	��B	��B	��B	�uB	�uB	�bB	�\B	�VB	�bB	�JB	�7B	�JB	�=B	�1B	�VB	�JB	�B	� B	s�B	~�B	�B	�%B	y�B	iyB	iyB	e`B	dZB	iyB	m�B	l�B	l�B	o�B	r�B	r�B	k�B	[#B	l�B	iyB	`BB	ffB	bNB	`BB	gmB	m�B	m�B	n�B	r�B	z�B	{�B	~�B	�B	�B	�%B	�7B	�PB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�B	�?B	�9B	�?B	�?B	�FB	�LB	�LB	�LB	�RB	�RB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�jB	�qB	�qB	�wB	�wB	�qB	�wB	�wB	��B	��B	��B	��B	��B	B	B	ÖB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�;B	�5B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�mB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
+B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB

=B
	7B
	7B
	7B
1B

=B

=B
DB

=B

=B
	7B
	7B
	7B

=B

=B
JB

=B
JB
VB
\B
bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
.B
2-B
7LB
;dB
D�B
J�B
Q�B
XB
]/B
bNB
gmB
k�B
n�B
q�B
w�B
}�B
� B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<e`B<#�
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
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250112012011312501120120113125011  AO  ARGQ                                                                        20111205112504  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112504  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125011  IP                  G�O�G�O�G�O�                