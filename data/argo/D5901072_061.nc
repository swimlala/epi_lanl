CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:55Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               =A   AO  20111130144126  20190522121829  1728_5048_061                   2C  D   APEX                            2142                            040306                          846 @��a���1   @��bW:�@4�p��
=�b�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyffD�3D�6fD�y�D�ɚD���D�<�D�ffD���D�� D�  D�L�DǼ�D�ٚD�<�D�c3D���D��3D��D�Y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@�33@�  A��A9��AY��Ay��A���A���A���A���A���A���A�  A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%� C'��C)��C+� C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�3C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<l�D<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF��DGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyL�D��fD�)�D�l�D���D�� D�0 D�Y�D�� D��3D�3D�@ Dǰ D���D�0 D�VfD���D��fD��D�L�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��#A��
A��A��#A��#A��/A��/A��#A��#A��;A��#A��;A��mA��yA��A��A��yA��yA��A��A��A��A��A��A��A��A��A��A���A�|�A�O�A�-A�ƨA���A�ȴA���A�-A�$�A��+A���A�|�A�$�A�&�A��A�A���A�  A�  A�  A�1A��A��A���A��mA�A���A�ȴA���A�A�S�A��A��FA�G�A�S�A�A�5?A�\)A�(�A���A�A�t�A� �A�l�A��;A�ȴA��A���A��;A�(�A�jA�A�{A��A�O�A�x�A�+A�{A���A�ffA��A�{A���A��A��!A���A���A���A��A�G�A�A�x�A�p�A���A��A�JA�33A�Q�A�=qA�~�A��uA�z�A��\A�n�A��A�dZA�bNA�r�A|�HA{&�AzI�Ax�/Av�+At{ArbNAlv�Aj1'Ai7LAg��Ad�`A_p�AY�7AV�/AT�jATZAS�wASoAR  AQ33APn�AO33ANI�AMt�AM;dALE�AL�AK�FAK�AHffAF�ADĜAB�uA?��A<9XA:��A9�
A8=qA6�9A5O�A4ffA3;dA2jA1O�A/��A.�A-��A-A+�A*�+A)�;A)p�A)K�A(��A(bA'�7A&��A%��A$�yA$��A$$�A#�A!��A �/AG�A�+AC�Av�A��AAz�AVAbA�PA��At�A�HAA�DA-A�mA�hAr�A�PA1'A�FA^5A�wA
��A	
=AC�A(�AO�AbA`BA��A+A �@��
@��@��h@��y@��9@�
=@�x�@���@���@�Z@��@���@���@�~�@�/@�;d@�=q@��@�x�@��;@�7@ߝ�@�@��@�&�@ץ�@�x�@� �@ҸR@���@�^5@̓u@�E�@���@��m@�;d@�v�@���@�/@ă@Å@��y@§�@�5?@��7@�p�@�j@�b@��w@��R@���@�/@�Q�@��F@��@�t�@�"�@�E�@���@��@��j@��@�@���@�v�@�$�@�{@��T@�hs@��@�Ĝ@�S�@���@���@�Ĝ@�Q�@�1'@��
@�+@��@���@�~�@�~�@�n�@�v�@��+@�v�@�M�@�J@��@��`@�Z@���@�J@�@�X@�/@��@���@�O�@�ȴ@�E�@��7@��@��j@�Z@�  @��w@���@�o@�V@���@��-@��@�-@��7@��@�j@�bN@� �@��P@�t�@��P@�t�@��@��y@�ff@�=q@�{@��#@��@�`B@�/@��@��`@��9@�Q�@�(�@��;@�l�@��@���@��R@�ȴ@���@��\@��!@��R@���@�E�@�=q@�^5@�^5@�=q@���@�`B@�bN@�b@��m@�|�@�K�@�;d@�"�@�@���@���@���@�v�@�ff@�V@�M�@�~�@���@�33@�l�@��@���@��@��@�33@��@��!@��+@�{@��#@�G�@��@�%@��`@�Ĝ@��@�Q�@�A�@�1'@� �@��m@�S�@���@�~�@�v�@�n�@��@�@���@�@���@�p�@�%@��9@��D@�Q�@�1@���@���@�;d@�o@��H@�ȴ@���@��R@��!@�ȴ@�ȴ@��R@��+@�J@��#@��T@���@�`B@��@���@��/@��j@���@�9X@�ƨ@�\)@�
=@���@��R@���@���@���@�-@���@��T@��^@�7L@���@��j@�9X@��m@��w@���@��@�;d@�@���@���@���@��\@��\@�n�@��@���@��@�&�@���@��D@�9X@�@K�@~�R@~v�@~E�@sƨ@l1@d�@]��@UV@K��@C�F@=��@9��@4�@/+@(�`@$�@��@ƨ@&�@��@
=@"�@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��#A��
A��A��#A��#A��/A��/A��#A��#A��;A��#A��;A��mA��yA��A��A��yA��yA��A��A��A��A��A��A��A��A��A��A���A�|�A�O�A�-A�ƨA���A�ȴA���A�-A�$�A��+A���A�|�A�$�A�&�A��A�A���A�  A�  A�  A�1A��A��A���A��mA�A���A�ȴA���A�A�S�A��A��FA�G�A�S�A�A�5?A�\)A�(�A���A�A�t�A� �A�l�A��;A�ȴA��A���A��;A�(�A�jA�A�{A��A�O�A�x�A�+A�{A���A�ffA��A�{A���A��A��!A���A���A���A��A�G�A�A�x�A�p�A���A��A�JA�33A�Q�A�=qA�~�A��uA�z�A��\A�n�A��A�dZA�bNA�r�A|�HA{&�AzI�Ax�/Av�+At{ArbNAlv�Aj1'Ai7LAg��Ad�`A_p�AY�7AV�/AT�jATZAS�wASoAR  AQ33APn�AO33ANI�AMt�AM;dALE�AL�AK�FAK�AHffAF�ADĜAB�uA?��A<9XA:��A9�
A8=qA6�9A5O�A4ffA3;dA2jA1O�A/��A.�A-��A-A+�A*�+A)�;A)p�A)K�A(��A(bA'�7A&��A%��A$�yA$��A$$�A#�A!��A �/AG�A�+AC�Av�A��AAz�AVAbA�PA��At�A�HAA�DA-A�mA�hAr�A�PA1'A�FA^5A�wA
��A	
=AC�A(�AO�AbA`BA��A+A �@��
@��@��h@��y@��9@�
=@�x�@���@���@�Z@��@���@���@�~�@�/@�;d@�=q@��@�x�@��;@�7@ߝ�@�@��@�&�@ץ�@�x�@� �@ҸR@���@�^5@̓u@�E�@���@��m@�;d@�v�@���@�/@ă@Å@��y@§�@�5?@��7@�p�@�j@�b@��w@��R@���@�/@�Q�@��F@��@�t�@�"�@�E�@���@��@��j@��@�@���@�v�@�$�@�{@��T@�hs@��@�Ĝ@�S�@���@���@�Ĝ@�Q�@�1'@��
@�+@��@���@�~�@�~�@�n�@�v�@��+@�v�@�M�@�J@��@��`@�Z@���@�J@�@�X@�/@��@���@�O�@�ȴ@�E�@��7@��@��j@�Z@�  @��w@���@�o@�V@���@��-@��@�-@��7@��@�j@�bN@� �@��P@�t�@��P@�t�@��@��y@�ff@�=q@�{@��#@��@�`B@�/@��@��`@��9@�Q�@�(�@��;@�l�@��@���@��R@�ȴ@���@��\@��!@��R@���@�E�@�=q@�^5@�^5@�=q@���@�`B@�bN@�b@��m@�|�@�K�@�;d@�"�@�@���@���@���@�v�@�ff@�V@�M�@�~�@���@�33@�l�@��@���@��@��@�33@��@��!@��+@�{@��#@�G�@��@�%@��`@�Ĝ@��@�Q�@�A�@�1'@� �@��m@�S�@���@�~�@�v�@�n�@��@�@���@�@���@�p�@�%@��9@��D@�Q�@�1@���@���@�;d@�o@��H@�ȴ@���@��R@��!@�ȴ@�ȴ@��R@��+@�J@��#@��T@���@�`B@��@���@��/@��j@���@�9X@�ƨ@�\)@�
=@���@��R@���@���@���@�-@���@��T@��^@�7L@���@��j@�9X@��m@��w@���@��@�;d@�@���@���@���@��\@��\@�n�@��@���@��@�&�@���@��D@�9X@�@K�@~�R@~v�@~E�@sƨ@l1@d�@]��@UV@K��@C�F@=��@9��@4�@/+@(�`@$�@��@ƨ@&�@��@
=@"�@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�FB
�LB
�jB
B
�#B%B8RB`BB�1BB��B��B�LB�XB��B�;B�BB{B�B �B%�B'�B-B5?B?}BH�BQ�BP�B[#B[#BhsBdZB\)BO�BE�BE�BF�BD�BB�B>wBB�BE�BM�BT�BVBO�BH�BC�B;dB5?B0!B-B-B!�B�BuB\BB��BB�B��B��BɺBŢB�NB�B�TBɺB��B��B�=Bw�BffB6FB%�B{BB
��B
�B
��B
�B
�=B
z�B
dZB
bNB
{�B
{�B
s�B
m�B
G�B
8RB
&�B
!�B
�B
B	��B	�;B	��B	��B	�hB	y�B	YB	+B��B�sB�B�
B��B��B��B��BŢBŢB��BĜBǮB��B�B��B	B�B�)B�TB�
BɺB�9B�B��B��B��B�{B�hB�VB�PB�DB�+B�7B�JB�JB�\B�\B�PB�VB�JB�DB�PB�VB�uB��B��B�hB�JB�JB�PB�bB�bB�DB�hB��B��B�uB�oB�hB�bB�PB�7B�B� B�B� B~�B~�B|�Bx�Bt�Bu�Bt�Bq�Bl�BffBt�BffBcTBaHBZB]/BbNBe`BP�BL�BH�BN�BXBO�BE�B@�BF�B6FB6FB8RB9XB:^B;dB2-B7LB33B49B0!B/B0!B0!B+B0!B-B-B1'B0!B2-B8RB7LB9XB>wB?}BA�B@�BA�BC�BE�BJ�BP�BP�BP�BP�BO�BO�BS�BQ�BYBbNBgmBq�By�Bs�Bx�B{�B}�B�B�B�B�B�DB�{B�oB�hB�uB�uB�{B��B��B��B��B��B��B�B�B�B�'B�LB�^B�qB�qB�}B��BÖBŢB��B��B�B�B�/B�;B�;B�BB�`B�`B�fB�mB�yB�ZB�#B�#B�5B�ZB�mB�mB�mB�mB�mB�B�B�B�B�B��B	B	  B��B��B	  B	B	B	%B	DB	VB	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	&�B	(�B	/B	8RB	:^B	=qB	A�B	C�B	F�B	J�B	M�B	P�B	[#B	_;B	cTB	gmB	hsB	hsB	hsB	gmB	gmB	hsB	jB	l�B	o�B	t�B	w�B	x�B	y�B	z�B	|�B	}�B	� B	�B	�=B	�bB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�LB	�LB	�qB	�jB	�qB	�}B	�wB	�}B	��B	��B	ÖB	B	ÖB	ŢB	ƨB	ÖB	ĜB	ƨB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
bB
�B
$�B
-B
49B
<jB
D�B
H�B
N�B
S�B
YB
^5B
cTB
gmB
iyB
o�B
t�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�FB
�RB
�qB
ÖB
�)B+B9XB`BB�1BÖB��B��B�^B�XB��B�HB�BB�B�B �B%�B'�B-B5?B?}BI�BQ�BP�B\)B[#BiyBffBaHBT�BG�BG�BJ�BF�BJ�BB�BC�BH�BP�BXBXBXBK�BI�B?}B:^B6FB1'B1'B%�B�B�B{B	7B��B%B�BB�B��BǮB�`B�B�sB��B��B��B�oB~�Bm�B;dB+B�B1B
��B
��B
�B
�^B
�bB
�B
jB
cTB
~�B
�B
z�B
z�B
S�B
>wB
)�B
&�B
#�B
JB	��B	�B	�B	��B	��B	� B	ffB	:^B	B�B�B�B��B��B��B��BɺBȴBĜBŢB��B��B�B	  B	
=B�B�HB�B�HB��B�RB�'B�B��B��B��B��B�hB�hB�hB�DB�JB�\B�bB�uB�hB�VB�\B�PB�VB�\B�bB��B��B��B�uB�\B�\B�bB��B�oB�\B�{B��B��B��B�uB�oB�oB�bB�PB�+B�B�B�B� B� B�B{�Bx�Bw�By�By�Bp�Bk�By�BiyBe`BdZB\)B_;BffBgmBR�BN�BJ�BR�B[#BR�BH�BC�BH�B:^B:^B:^B;dB<jB=qB5?B9XB49B5?B33B33B33B33B.B49B0!B0!B33B2-B5?B<jB:^B=qBA�BA�BB�BB�BB�BD�BF�BL�BQ�BQ�BQ�BQ�BO�BP�BT�BR�BZBcTBhsBr�Bz�Bs�By�B|�B� B�B�B�%B�%B�PB��B�uB�oB�uB�{B��B��B��B��B��B��B��B�B�B�!B�-B�RB�dB�qB�qB�}B��BÖBŢB��B��B�
B�#B�5B�NB�HB�BB�fB�fB�sB�B�B�sB�)B�/B�;B�`B�sB�sB�sB�sB�sB�B�B�B�B�B��B	%B	B��B��B	B	B	B	%B	JB	\B	PB	\B	oB	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	'�B	)�B	0!B	9XB	:^B	=qB	B�B	C�B	F�B	J�B	M�B	Q�B	[#B	_;B	cTB	hsB	iyB	iyB	jB	hsB	hsB	hsB	k�B	l�B	o�B	t�B	w�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�7B	�bB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�RB	�RB	�qB	�jB	�qB	�}B	�}B	��B	��B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ÖB	ĜB	ǮB	ɺB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�B	�#B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�BB	�BB	�HB	�TB	�TB	�ZB	�ZB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
bB
�B
$�B
-B
49B
<jB
D�B
I�B
N�B
S�B
ZB
^5B
dZB
gmB
iyB
o�B
t�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452142012011014521420120110145214  AO  ARGQ                                                                        20111130144126  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144126  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145214  IP                  G�O�G�O�G�O�                