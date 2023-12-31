CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:35Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               *A   AO  20111205113036  20190522121836  1901_5055_042                   2C  D   APEX                            2140                            040306                          846 @ԑ�P��w1   @ԑ�    @-�1&�x��c`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  @���AffA@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dmy�Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy��D�	�D�<�D�s3D�� D���D�#3D���D���D���D�,�D���Dǹ�D���D�#3D�|�D� D��fD��D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�33@�  A  A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&��B.  B6  B>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB���B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C�3C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI� DJffDJ�fDKffDK�fDLffDL� DMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDl` Dl� Dm` Dm�fDnffDn� DoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwY�Dy� D���D�0 D�ffD��3D�� D�fD�|�D�� D�� D�  D�|�DǬ�D�� D�fD�p D�3D�ٚD��D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�VA�JA�  A��A��A�ĜAۼjA۩�Aۗ�Aۏ\A�~�A�x�A�r�A�jA�ffA�bNA�VA�E�A�A���A׶FA��AՋDA�&�A�VA�jA���Aδ9A�p�A��A���A��A�=qAũ�A�?}Aç�A�E�A���AuA�S�A��A�A���A���A�A�A�A���A���A�ZA���A�&�A��A�`BA���A�jA��A�|�A�{A�bNA��A�v�A��;A�r�A��-A��PA�A��\A�A��A���A��A�^5A��wA� �A��+A��!A�bNA���A�$�A��A��A�33A�(�A�$�A�A�A���A��A���A��;A�S�A�K�A~A|{AzM�Au�PAq�Am�^Aj(�Ah�Agx�Ac\)A`ȴA_�FA]�;A[��A[+A[ƨAW�AR�yAR �AQ�AO�ANȴAL��AJ�\AIhsAH�HAHE�AGC�AEƨADAE�ADȴABr�A@ �A=��A9K�A6A1�A0r�A/�A.VA-ƨA-&�A,=qA*�9A*�A)�PA(�A&�A#A"�uA#
=A#%A"E�A!A!`BA!%A 5?A��Ap�A+AhsA �A ^5A bNA 5?A Q�A �AXA�
AdZA�A��A"�AVAz�A�A;dAhsA�uA��A�A
=A�A��A�`A
=AffA�`A�jA�`A��A�!A��A��A��A-A�wA��A�A`BAO�A?}A�A%AVA��AffA�#A�;AbA5?A�A
��A	��A	
=A	oA��A  Ap�A�AȴAĜAE�AA�TA�^AdZA-A�
A�-Al�A/A�A��AbNA5?A�mA\)A ��A �!A (�@�|�@�{@�V@���@��-@���@��y@��#@��/@�dZ@�;d@���@��@�`B@�j@���@�@�E�@���@�hs@��@�u@���@�R@���@�@��m@�o@��T@�^@�x�@�@��@�C�@�"�@��@�=q@�^@�%@��`@�@�I�@�t�@���@�5?@ݡ�@݁@��
@ڗ�@�~�@�$�@���@ف@؋D@�1'@��;@�ƨ@�
=@�~�@��#@ԓu@�b@��;@ӍP@ҸR@�{@��#@��@ѩ�@�X@�/@���@Ь@��m@�K�@�^5@�p�@���@���@���@̣�@̋D@�Q�@��;@�@�M�@�O�@Ȭ@� �@��
@Ǖ�@Ǖ�@�33@���@Ƨ�@�n�@��@ŉ7@�V@�1'@å�@��@�^5@��@��T@��h@�%@���@�r�@�I�@��m@��F@���@�l�@�@�M�@���@�x�@�7L@��`@�A�@��@�l�@�K�@�+@��@�E�@���@�&�@���@��u@� �@���@��H@���@��T@��`@��@�9X@�+@�ff@��@��T@�@�hs@���@�r�@�A�@���@���@�K�@���@�E�@�@��-@�p�@���@�r�@�  @�t�@�33@���@�ȴ@�~�@��@���@��@�V@��@��@��`@�%@�G�@���@��u@���@���@��D@�Z@�9X@�  @�@��+@�-@�@�%@��@�j@��w@�t�@�;d@���@��!@�M�@�$�@�@�%@��@�1@��w@�S�@�@�ȴ@���@�~�@�V@���@��@��u@�Z@��@���@��w@�|�@�
=@���@�~�@�^5@�E�@�$�@��@���@��7@�x�@�X@�G�@��@�V@��j@��@�Z@�  @��w@�l�@��@��H@���@�^5@�J@���@�?}@�I�@�1@���@�t�@�C�@�o@�ȴ@���@��!@���@��H@��@��@�ȴ@��+@�@��^@���@��/@��^@�1'@}O�@s��@j�@`A�@W��@O��@D��@<��@6{@/\)@(�`@#"�@�/@|�@�@�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�VA�JA�  A��A��A�ĜAۼjA۩�Aۗ�Aۏ\A�~�A�x�A�r�A�jA�ffA�bNA�VA�E�A�A���A׶FA��AՋDA�&�A�VA�jA���Aδ9A�p�A��A���A��A�=qAũ�A�?}Aç�A�E�A���AuA�S�A��A�A���A���A�A�A�A���A���A�ZA���A�&�A��A�`BA���A�jA��A�|�A�{A�bNA��A�v�A��;A�r�A��-A��PA�A��\A�A��A���A��A�^5A��wA� �A��+A��!A�bNA���A�$�A��A��A�33A�(�A�$�A�A�A���A��A���A��;A�S�A�K�A~A|{AzM�Au�PAq�Am�^Aj(�Ah�Agx�Ac\)A`ȴA_�FA]�;A[��A[+A[ƨAW�AR�yAR �AQ�AO�ANȴAL��AJ�\AIhsAH�HAHE�AGC�AEƨADAE�ADȴABr�A@ �A=��A9K�A6A1�A0r�A/�A.VA-ƨA-&�A,=qA*�9A*�A)�PA(�A&�A#A"�uA#
=A#%A"E�A!A!`BA!%A 5?A��Ap�A+AhsA �A ^5A bNA 5?A Q�A �AXA�
AdZA�A��A"�AVAz�A�A;dAhsA�uA��A�A
=A�A��A�`A
=AffA�`A�jA�`A��A�!A��A��A��A-A�wA��A�A`BAO�A?}A�A%AVA��AffA�#A�;AbA5?A�A
��A	��A	
=A	oA��A  Ap�A�AȴAĜAE�AA�TA�^AdZA-A�
A�-Al�A/A�A��AbNA5?A�mA\)A ��A �!A (�@�|�@�{@�V@���@��-@���@��y@��#@��/@�dZ@�;d@���@��@�`B@�j@���@�@�E�@���@�hs@��@�u@���@�R@���@�@��m@�o@��T@�^@�x�@�@��@�C�@�"�@��@�=q@�^@�%@��`@�@�I�@�t�@���@�5?@ݡ�@݁@��
@ڗ�@�~�@�$�@���@ف@؋D@�1'@��;@�ƨ@�
=@�~�@��#@ԓu@�b@��;@ӍP@ҸR@�{@��#@��@ѩ�@�X@�/@���@Ь@��m@�K�@�^5@�p�@���@���@���@̣�@̋D@�Q�@��;@�@�M�@�O�@Ȭ@� �@��
@Ǖ�@Ǖ�@�33@���@Ƨ�@�n�@��@ŉ7@�V@�1'@å�@��@�^5@��@��T@��h@�%@���@�r�@�I�@��m@��F@���@�l�@�@�M�@���@�x�@�7L@��`@�A�@��@�l�@�K�@�+@��@�E�@���@�&�@���@��u@� �@���@��H@���@��T@��`@��@�9X@�+@�ff@��@��T@�@�hs@���@�r�@�A�@���@���@�K�@���@�E�@�@��-@�p�@���@�r�@�  @�t�@�33@���@�ȴ@�~�@��@���@��@�V@��@��@��`@�%@�G�@���@��u@���@���@��D@�Z@�9X@�  @�@��+@�-@�@�%@��@�j@��w@�t�@�;d@���@��!@�M�@�$�@�@�%@��@�1@��w@�S�@�@�ȴ@���@�~�@�V@���@��@��u@�Z@��@���@��w@�|�@�
=@���@�~�@�^5@�E�@�$�@��@���@��7@�x�@�X@�G�@��@�V@��j@��@�Z@�  @��w@�l�@��@��H@���@�^5@�J@���@�?}@�I�@�1@���@�t�@�C�@�o@�ȴ@���@��!@���@��H@��@��@�ȴ@��+@�@��^@���@��/@��^@�1'@}O�@s��@j�@`A�@W��@O��@D��@<��@6{@/\)@(�`@#"�@�/@|�@�@�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
t�B
u�B
v�B
v�B
t�B
t�B
s�B
s�B
t�B
t�B
s�B
t�B
x�B
�uB
�5B
��B#�B �B
��B
�;B
��B
��B
��B
��B
�=B
�%B
�B
�hB
��B
�-B
�qB
��B
��B
�
B
�yB9XBn�B� B��B�9B�BB��BBhB&�B<jBVB_;Bo�B� B�B�%B�+B�+B�+B�+B�%B�B� B~�Bv�Bo�Bq�Bl�Bl�Bn�BhsB_;BF�B&�BPB+B�B��B�JB)�B
��B
��B
�PB
z�B
C�B
�B
B	�5B	ĜB	�B	��B	�7B	jB	VB	>wB	.B	+B	�B	PB	B��B�B�`B��B	�B	B�B�ZB�NB�`B�mB�ZB�B�B��B	DB	VB	JB	oB	J�B	W
B	P�B	E�B	5?B	�B��B�B�NB�TB�B�B�B�B�B�B�B�B�HB�B�/B��B	
=B	�B	#�B	&�B	'�B	.B	@�B	K�B	XB	q�B	�+B	��B	�9B	�RB	�}B	ÖB	ĜB	ȴB	��B	ÖB	��B	��B	�
B	��B	��B	��B	��B	ȴB	�wB	�3B	�B	��B	��B	��B	�B	�B	��B	�B	�XB	ǮB	ȴB	ɺB	��B	��B	ɺB	ɺB	��B	��B	��B	�
B	�/B	�NB	�B	�B	�B	�B	��B	��B
B
+B
  B	��B	��B	��B
B
B	��B	��B	��B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�fB	�mB	�`B	�`B	�mB	�mB	�fB	�`B	�`B	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�ZB	�TB	�BB	�;B	�/B	�5B	�5B	�5B	�5B	�)B	�)B	�5B	�;B	�;B	�;B	�BB	�NB	�TB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�HB	�fB	�yB	�B	�B	�B	�yB	�mB	�fB	�fB	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B

=B
DB
JB
DB
DB

=B

=B
DB

=B
DB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
bB
bB
bB
hB
hB
oB
oB
hB
hB
hB
hB
oB
oB
hB
hB
hB
hB
bB
hB
hB
hB
hB
hB
oB
uB
{B
�B
{B
{B
{B
uB
uB
uB
uB
oB
oB
hB
bB
hB
bB
bB
bB
bB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
-B
33B
:^B
=qB
C�B
I�B
N�B
VB
\)B
`BB
dZB
iyB
m�B
r�B
w�B
y�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
t�B
u�B
v�B
v�B
t�B
t�B
s�B
s�B
t�B
t�B
s�B
u�B
z�B
��B
�TB
��B)�B(�B%B
�fB
��B
�B
ƨB
��B
�JB
�1B
�PB
��B
�B
�9B
�}B
��B
��B
�B
�B?}Bq�B�B��B�qB�`B��B1B�B+B?}BW
BaHBq�B�B�B�7B�7B�7B�=B�7B�=B�7B�B�Bz�Bq�Bt�Br�Bo�Br�Bl�BhsBP�B.BhBVB�B��B��B6FB
�/B
��B
�oB
�=B
Q�B
�B
PB	�sB	��B	�!B	��B	��B	u�B	aHB	H�B	2-B	0!B	)�B	uB	B��B��B�mB��B	#�B	VB�B�mB�mB�fB�B�B�B��B��B	VB	uB	hB	\B	L�B	^5B	XB	N�B	B�B	#�B	
=B�B�`B�`B�B�B�B��B�B�B�B�B�sB�#B�)B��B	JB	�B	$�B	'�B	+B	0!B	A�B	L�B	W
B	o�B	�%B	��B	�?B	�RB	��B	ƨB	ɺB	��B	��B	ǮB	�}B	��B	�B	�
B	�
B	�B	��B	��B	B	�FB	�B	�B	��B	��B	�!B	�3B	�B	�B	�^B	ȴB	ȴB	ɺB	��B	��B	��B	ɺB	��B	��B	��B	�
B	�5B	�NB	�B	�B	�B	��B	��B	��B
B
	7B
B
B
  B	��B
B
B
B
  B	��B	��B
+B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�yB	�mB	�yB	�`B	�fB	�sB	�sB	�mB	�fB	�fB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�`B	�`B	�HB	�HB	�;B	�;B	�;B	�BB	�;B	�/B	�)B	�;B	�BB	�BB	�BB	�BB	�TB	�ZB	�ZB	�TB	�ZB	�`B	�`B	�mB	�fB	�ZB	�`B	�ZB	�TB	�TB	�NB	�mB	�yB	�B	�B	�B	�B	�sB	�mB	�mB	�mB	�fB	�mB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
+B
+B
+B
+B
%B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
DB
JB
JB
JB
PB
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
oB
oB
oB
oB
oB
oB
oB
hB
hB
oB
oB
hB
hB
hB
hB
oB
hB
uB
{B
�B
�B
�B
�B
�B
{B
{B
uB
uB
oB
uB
uB
bB
oB
hB
hB
hB
hB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
-B
33B
:^B
>wB
C�B
I�B
O�B
VB
\)B
`BB
dZB
iyB
n�B
s�B
w�B
z�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<T��<#�
<D��<#�
<49X<#�
<#�
<D��<49X<49X<#�
<#�
<#�
<49X<#�
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
<T��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250222012011312502220120113125022  AO  ARGQ                                                                        20111205113036  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113036  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125022  IP                  G�O�G�O�G�O�                