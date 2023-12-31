CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:52Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               0A   AO  20111130144002  20190522121829  1728_5048_048                   2C  D   APEX                            2142                            040306                          846 @Ԣ��
1   @Ԣ��?�@5�^5?|��cV�u1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  DwffDy�fD��3D�33D���D���D���D�33D�Y�D��fD�� D��D��fDǦfD��3D� Dڃ3D๚D��D��D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBn��BvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	�3C�3C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"��D#l�D#��D$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh��Dil�Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwL�Dyl�D��fD�&fD���D�� D�� D�&fD�L�D���D��3D��D�y�DǙ�D��fD�3D�vfD��D���D��D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AǼjA���A���A���A���A�A�A�A�ĜA�A�ƨA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA�ȴA���A���A���A���A��
A��
A��A��
A���A��
A��
A��
Aǉ7A���AŲ-A��
A�ZA��#A�A��A��\A��TA�\)A��A��mA�dZA�;dA�ĜA�ZA�ƨA�p�A��A��A��A�A��A�A�E�A���A�-A�|�A���A�Q�A�v�A��FA��A�&�A�dZA��/A�bNA��`A���A��\A�+A�~�A�(�A�ȴA�=qA�K�A��+A��7A���A�+A���A�oA�1A��PA��A��A��;A�1A�C�A��A�`BA��A���A�33A��hA��A�=qA�  A}�;Az��AzȴAz(�Ax��Awp�Av��At�HAs33Ap��An��Ak��Aj1'Ah��Ag�7Af��AeAa��A`��A`�A]G�A[�AZ1'AX��AU7LAPjANM�AJ�!AH�!AF��AD=qAB�9AAS�A?��A>��A=A<ffA;�A:��A9�hA7�A5�#A4ffA2�HA2=qA1�A/
=A.(�A-�mA,ĜA+�A+&�A)�PA)oA'�PA%�
A$��A#�-A"ȴA"E�A!ƨA ��A �A&�A-AS�AZA�A��A��A9XAl�A��A�7A��A�A��A�At�A��A=qA��AO�A33A��A(�A��Ax�A��A=qAl�A
�A��A��A?}AbAoA �A��A�AjA��A`BA&�A �A 5?@��w@�O�@�v�@���@�@��`@�F@��T@���@�S�@�7L@�I�@���@���@�P@�M�@�x�@�&�@㝲@��@��m@�S�@ޗ�@��@�p�@ܬ@�  @�t�@�dZ@�33@�n�@�n�@�E�@�=q@�n�@�$�@ّh@���@�ƨ@���@ָR@�5?@Ձ@���@��#@��/@Ѓ@��@ϥ�@��@́@˶F@�+@�ȴ@�-@��#@ɩ�@�G�@���@���@�\)@��@��@���@���@�A�@�5?@�Z@�5?@���@�x�@�hs@�/@�&�@��@��@��@��@�V@�bN@�v�@�-@�n�@���@��@�1@��@�M�@��@���@�&�@��#@�V@�V@�A�@���@�O�@��^@�/@�G�@�I�@�;d@�C�@�\)@���@��@�K�@�@�ȴ@���@�n�@���@�E�@���@��
@�l�@�|�@��P@�I�@�hs@�p�@���@�K�@�J@��w@�V@���@��@��/@���@�p�@���@��u@�7L@��7@�`B@��@��
@�33@��y@�=q@�J@�n�@�v�@�$�@��#@�V@���@��D@��D@��`@��D@�Q�@�  @���@��F@��F@���@�t�@�S�@��@���@�ȴ@��!@�n�@�5?@���@�G�@���@���@��`@��`@���@�A�@�  @�dZ@��H@��@�`B@��D@��;@��P@�dZ@���@��w@�|�@�;d@��@���@���@��#@�7L@�/@�V@��`@��D@��m@�S�@��@�~�@�=q@��@��^@�hs@�G�@��@���@��@�Ĝ@�z�@�9X@�1'@�1'@�(�@� �@� �@�b@���@��
@��@���@��@��w@��w@���@�t�@�K�@��@���@�v�@�~�@���@��\@�5?@�@�@��@�/@�j@��@��@��;@��
@��;@��
@��P@�C�@�33@�o@�@���@��H@��R@�v�@�$�@��@�p�@��/@��j@��@�j@�A�@�b@��;@��w@���@�\)@�@�n�@�@��-@�hs@�X@�G�@��@���@��`@��@�z�@�Z@�A�@�b@���@���@~@t��@m�@d�@]�@T(�@K��@Ep�@?
=@9G�@3dZ@-�T@)��@%p�@ ��@�/@�9@(�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AǼjA���A���A���A���A�A�A�A�ĜA�A�ƨA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA�ȴA���A���A���A���A��
A��
A��A��
A���A��
A��
A��
Aǉ7A���AŲ-A��
A�ZA��#A�A��A��\A��TA�\)A��A��mA�dZA�;dA�ĜA�ZA�ƨA�p�A��A��A��A�A��A�A�E�A���A�-A�|�A���A�Q�A�v�A��FA��A�&�A�dZA��/A�bNA��`A���A��\A�+A�~�A�(�A�ȴA�=qA�K�A��+A��7A���A�+A���A�oA�1A��PA��A��A��;A�1A�C�A��A�`BA��A���A�33A��hA��A�=qA�  A}�;Az��AzȴAz(�Ax��Awp�Av��At�HAs33Ap��An��Ak��Aj1'Ah��Ag�7Af��AeAa��A`��A`�A]G�A[�AZ1'AX��AU7LAPjANM�AJ�!AH�!AF��AD=qAB�9AAS�A?��A>��A=A<ffA;�A:��A9�hA7�A5�#A4ffA2�HA2=qA1�A/
=A.(�A-�mA,ĜA+�A+&�A)�PA)oA'�PA%�
A$��A#�-A"ȴA"E�A!ƨA ��A �A&�A-AS�AZA�A��A��A9XAl�A��A�7A��A�A��A�At�A��A=qA��AO�A33A��A(�A��Ax�A��A=qAl�A
�A��A��A?}AbAoA �A��A�AjA��A`BA&�A �A 5?@��w@�O�@�v�@���@�@��`@�F@��T@���@�S�@�7L@�I�@���@���@�P@�M�@�x�@�&�@㝲@��@��m@�S�@ޗ�@��@�p�@ܬ@�  @�t�@�dZ@�33@�n�@�n�@�E�@�=q@�n�@�$�@ّh@���@�ƨ@���@ָR@�5?@Ձ@���@��#@��/@Ѓ@��@ϥ�@��@́@˶F@�+@�ȴ@�-@��#@ɩ�@�G�@���@���@�\)@��@��@���@���@�A�@�5?@�Z@�5?@���@�x�@�hs@�/@�&�@��@��@��@��@�V@�bN@�v�@�-@�n�@���@��@�1@��@�M�@��@���@�&�@��#@�V@�V@�A�@���@�O�@��^@�/@�G�@�I�@�;d@�C�@�\)@���@��@�K�@�@�ȴ@���@�n�@���@�E�@���@��
@�l�@�|�@��P@�I�@�hs@�p�@���@�K�@�J@��w@�V@���@��@��/@���@�p�@���@��u@�7L@��7@�`B@��@��
@�33@��y@�=q@�J@�n�@�v�@�$�@��#@�V@���@��D@��D@��`@��D@�Q�@�  @���@��F@��F@���@�t�@�S�@��@���@�ȴ@��!@�n�@�5?@���@�G�@���@���@��`@��`@���@�A�@�  @�dZ@��H@��@�`B@��D@��;@��P@�dZ@���@��w@�|�@�;d@��@���@���@��#@�7L@�/@�V@��`@��D@��m@�S�@��@�~�@�=q@��@��^@�hs@�G�@��@���@��@�Ĝ@�z�@�9X@�1'@�1'@�(�@� �@� �@�b@���@��
@��@���@��@��w@��w@���@�t�@�K�@��@���@�v�@�~�@���@��\@�5?@�@�@��@�/@�j@��@��@��;@��
@��;@��
@��P@�C�@�33@�o@�@���@��H@��R@�v�@�$�@��@�p�@��/@��j@��@�j@�A�@�b@��;@��w@���@�\)@�@�n�@�@��-@�hs@�X@�G�@��@���@��`@��@�z�@�Z@�A�@�b@���@���@~@t��@m�@d�@]�@T(�@K��@Ep�@?
=@9G�@3dZ@-�T@)��@%p�@ ��@�/@�9@(�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�FB�FB�FB�?B�?B�?B�FB�FB�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�FB�FB�FB�FB�FB�FB�FB�LB�RB�^B�dBB��B��B��B�B��B�B�HB�;B�TB�B�
B��B��B��B��B��BŢBÖB��B�RB�?B�9B�9B�B��B��B��B��B�uB�\B�%B�B�Bl�B^5B]/BS�BJ�B;dB0!B%�B�BhBhBB��B�B�NB�BɺB�qB��B��B�%Bq�BaHBO�BA�B5?B'�BuBB
��B
�
B
B
��B
r�B
C�B
�B
+B
�B
�B
,B
)�B
&�B
hB
B
	7B	��B	�fB	�5B	�ZB	��B	��B	B	�!B	��B	��B	�oB	�B	|�B	m�B	N�B	!�B	VB�B�HB��B��B�FB�B��B��B�oB�VB�PB��B��B��B��B�VB�B}�B|�B�1B�%B�B�PB�B� B�PBs�Bk�Bq�Bt�Bs�Bx�Bs�Bo�Bo�Bo�Bx�Bw�Bq�Bq�Bn�Bm�BjBiyBgmBgmBhsBdZBcTBdZBcTBbNBbNBaHB^5B_;B_;B^5B^5B\)B\)B[#BXB\)BR�BQ�BQ�BR�BO�BN�BK�BL�BO�BN�BI�BI�BG�BF�BJ�BI�BI�BO�BN�BI�BG�BM�BM�BE�BH�BD�BE�BJ�B@�B@�B>wB=qB9XB6FB=qB9XB8RB8RB9XB:^B>wB@�BB�BF�BI�BL�BP�BW
BcTBffBhsBl�Bq�Bv�By�By�Bz�B|�B{�By�By�By�B}�B{�B{�B� B�%B�B�B�+B�+B�B�B�B�PB�=B�bB��B��B��B�7B�uB�PB�PB�B�=B�PB�\B�\B�\B�\B�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�!B�RBĜBŢBŢB��B�B�BB�;B�NB�TB�HB�mB�B�B�B��B��B��B	B	1B	B	  B	VB	�B	�B	�B	!�B	#�B	33B	=qB	=qB	H�B	@�B	@�B	:^B	!�B	\B	PB	PB		7B	PB	+B	1'B	;dB	=qB	A�B	B�B	B�B	B�B	D�B	E�B	I�B	L�B	O�B	P�B	M�B	L�B	N�B	O�B	T�B	W
B	YB	]/B	`BB	aHB	aHB	cTB	jB	m�B	n�B	p�B	r�B	t�B	u�B	v�B	x�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	~�B	~�B	� B	�B	�1B	�DB	�JB	�JB	�VB	�\B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�3B	�?B	�FB	�RB	�RB	�dB	�}B	��B	B	B	ÖB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
JB
�B
�B
(�B
0!B
8RB
<jB
D�B
I�B
O�B
S�B
XB
^5B
bNB
ffB
jB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�FB�FB�FB�?B�?B�?B�FB�FB�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�FB�FB�FB�FB�FB�FB�FB�LB�RB�^B�jBŢB�B�mB�fB�`B�
B�#B�`B�NB�fB�HB�BB�B��B��B��B��BǮBŢBǮB�qB�XB�XB�^B�B�B��B��B��B��B�{B�DB�%B�Bp�B`BB_;BVBN�B=qB6FB(�B�BuB{B+B��B��B�`B�/B��B��B�B��B�PBw�BgmBT�BE�B9XB.B�B+B  B
�BB
��B
�B
�B
O�B
'�B
1B
�B
#�B
0!B
,B
+B
{B
%B
VB
  B	�B	�NB	�mB	��B	��B	��B	�3B	��B	��B	��B	�%B	�B	v�B	[#B	'�B	�B�B�mB�#BŢB�^B�'B��B��B��B�oB�\B��B��B��B��B�uB�%B�B�B�=B�+B�%B�\B�%B�B�\Bw�Bp�Bu�Bw�Bv�Bz�Bu�Br�Br�Br�B{�Bz�Bt�Bv�Bs�Bp�Bl�Bl�BjBk�BjBhsBffBhsBe`BdZBdZBcTB_;B`BB`BBaHB_;B^5B_;B]/B[#BaHBXBS�BT�BW
BS�BQ�BM�BO�BQ�BP�BJ�BJ�BH�BG�BK�BL�BM�BR�BP�BK�BI�BP�BO�BG�BK�BF�BG�BL�BD�BB�B@�B>wB<jB:^B?}B:^B9XB9XB:^B<jB?}BA�BB�BG�BK�BL�BQ�BW
BcTBgmBiyBn�Bs�Bx�Bz�Bz�B|�B� B� B{�Bz�Bz�B~�B|�B}�B�B�+B�B�B�1B�1B�B�B�%B�VB�JB�oB��B��B��B�JB��B�PB�VB�%B�=B�VB�\B�\B�\B�\B�\B�oB��B��B��B��B��B�B��B�B�B�B��B��B�B�RBƨBƨBŢB��B�B�HB�;B�ZB�`B�HB�mB�B�B�B��B��B��B	B	PB	%B��B	VB	�B	�B	�B	!�B	"�B	33B	>wB	?}B	J�B	C�B	D�B	:^B	#�B	bB	PB	PB	+B	DB	)�B	1'B	<jB	?}B	B�B	C�B	C�B	C�B	E�B	E�B	I�B	M�B	P�B	R�B	N�B	L�B	N�B	O�B	VB	XB	ZB	^5B	`BB	aHB	aHB	dZB	k�B	n�B	o�B	p�B	r�B	u�B	v�B	w�B	y�B	{�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�B	� B	� B	� B	�B	�1B	�JB	�PB	�PB	�\B	�bB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�3B	�?B	�FB	�XB	�RB	�jB	��B	B	B	B	ÖB	ƨB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
JB
�B
�B
(�B
0!B
8RB
<jB
D�B
I�B
O�B
S�B
YB
^5B
bNB
ffB
jB
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<�9X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452092012011014520920120110145209  AO  ARGQ                                                                        20111130144002  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144002  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145210  IP                  G�O�G�O�G�O�                