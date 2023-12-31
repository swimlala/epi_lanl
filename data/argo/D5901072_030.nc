CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:47Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143806  20190522121828  1728_5048_030                   2C  D   APEX                            2142                            040306                          846 @�ui� 1   @�ui���	@5���$��cc�
=p�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C�fC  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyl�D��D�)�D�p D���D�fD�,�D�y�D��fD��3D�3D�� Dǰ D���D�&fDڃ3D��3D���D��D�VfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33C� C��C��C��C	�3C�3C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO� CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	��D
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8� D9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKl�DK��DLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWl�DW�fDXffDX�fDYffDY�fDZffDZ� D[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDel�De�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDj` Dj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDyS3D�  D��D�c3D�� D���D�  D�l�D���D��fD�fD�s3Dǣ3D�� D��D�vfD�fD�� D� D�I�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�1'A���A�/A��^A�dZA��A�VA��FA��A��;A���A�=qA��`A��A�`BA���A�ĜA��;A�oA�`BA��#A�+A�p�A���A�5?A�jA��A�ȴA���A�bNA�A���A�JA�|�A��A��A��FA�ffA��HA�A�A��\A�/A��A�^5A�VA�ƨA��PA�\)A�?}A�+A��A�JA�%A��A���A��9A��A�;dA��A�dZA�1A��^A�~�A��A�C�A��TA�ƨA���A�=qA��A�/A�hsA�C�A�z�A���A���A�~�A��;A�K�A��DA�|�A�  A��A���A��TA��FA��9A�%A�hsA�VA�G�A�&�A�=qA�K�A~r�A{O�Ayl�Aw�7At�uAsAr��Ar�DArbAq�-AqXAn��Ah��Ae��Ac�-AbE�Aa��AaoA`��A`A^��A\5?AZZAXz�AU+AQ�
AO|�AN��AN$�AM�ALVAKVAJ �AHA�AE�A@��A=�wA<E�A;�A:M�A9�wA9C�A8�RA81A7p�A65?A3VA1A0�yA0JA/�#A/t�A.�/A.jA-�-A,�`A,r�A,M�A+��A+�A)`BA(M�A&ĜA&M�A#�^A!��A ��A bAx�A��A�\A�wA��A33A��A�yA��A�7AVAv�A��A�A1'A��A{A�A�;A��AdZA��A��AO�A��A�TA
I�A	x�A��A��A��A�A�`A�A�+A ��@�C�@�?}@�9X@�bN@���@���@�1@���@�\)@�5?@�p�@�9@�l�@�V@�/@� �@旍@�?}@� �@� �@���@ݲ-@܋D@ە�@ڟ�@٩�@���@�r�@��
@�33@�Ĝ@ѡ�@д9@���@�~�@̬@��@�S�@�ff@ȣ�@�@�x�@�ff@�{@��j@�bN@� �@��;@�l�@�^5@�G�@�+@���@�Z@���@���@��T@�7L@��
@�C�@��@���@���@�ff@���@��@�X@�V@��@���@��@�n�@�V@�{@��T@�@�x�@�X@�&�@��@��@���@��`@�Ĝ@���@���@�J@��H@�+@�S�@��@��@���@��
@��j@�p�@��#@��@�=q@�5?@���@�%@�1@�ȴ@�@���@���@�p�@��@���@��@��T@�bN@�I�@�b@��-@���@�r�@��#@�  @��R@��;@��/@�-@�+@���@�b@�1@��w@�\)@���@�V@�-@��@�`B@�z�@�b@��
@���@�C�@��H@�=q@���@��`@�  @��
@���@�
=@��\@�-@���@��7@�7L@�Ĝ@�A�@���@�33@�ȴ@��+@�~�@�^5@��@��T@��9@��!@�{@� �@���@��@�?}@�p�@���@���@���@��h@���@��/@�1'@���@�\)@�K�@��@��@�/@�~�@��!@�5?@��@��@��@�;d@�"�@�+@�Z@�hs@�5?@�ff@�M�@�G�@�j@��-@���@�=q@�b@���@���@�9X@�V@�G�@��@�r�@�bN@��;@���@�v�@���@�&�@�Ĝ@�  @�+@��@�Z@� �@�  @��@��@���@�C�@��@��y@��H@�"�@�K�@�@��H@�v�@�$�@��T@��h@�p�@�G�@��@��@���@�(�@��
@�"�@�ff@�@��^@��-@���@�O�@��@�%@��/@���@�bN@�9X@��m@���@���@��@�t�@�C�@��@���@�$�@��#@��#@���@�@�{@�$�@�5?@�=q@�G�@v�R@o\)@fff@^{@U�@L�/@C��@<j@5�T@0�@-@)�^@"�@/@�@�@�9@9X@��@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�1'A���A�/A��^A�dZA��A�VA��FA��A��;A���A�=qA��`A��A�`BA���A�ĜA��;A�oA�`BA��#A�+A�p�A���A�5?A�jA��A�ȴA���A�bNA�A���A�JA�|�A��A��A��FA�ffA��HA�A�A��\A�/A��A�^5A�VA�ƨA��PA�\)A�?}A�+A��A�JA�%A��A���A��9A��A�;dA��A�dZA�1A��^A�~�A��A�C�A��TA�ƨA���A�=qA��A�/A�hsA�C�A�z�A���A���A�~�A��;A�K�A��DA�|�A�  A��A���A��TA��FA��9A�%A�hsA�VA�G�A�&�A�=qA�K�A~r�A{O�Ayl�Aw�7At�uAsAr��Ar�DArbAq�-AqXAn��Ah��Ae��Ac�-AbE�Aa��AaoA`��A`A^��A\5?AZZAXz�AU+AQ�
AO|�AN��AN$�AM�ALVAKVAJ �AHA�AE�A@��A=�wA<E�A;�A:M�A9�wA9C�A8�RA81A7p�A65?A3VA1A0�yA0JA/�#A/t�A.�/A.jA-�-A,�`A,r�A,M�A+��A+�A)`BA(M�A&ĜA&M�A#�^A!��A ��A bAx�A��A�\A�wA��A33A��A�yA��A�7AVAv�A��A�A1'A��A{A�A�;A��AdZA��A��AO�A��A�TA
I�A	x�A��A��A��A�A�`A�A�+A ��@�C�@�?}@�9X@�bN@���@���@�1@���@�\)@�5?@�p�@�9@�l�@�V@�/@� �@旍@�?}@� �@� �@���@ݲ-@܋D@ە�@ڟ�@٩�@���@�r�@��
@�33@�Ĝ@ѡ�@д9@���@�~�@̬@��@�S�@�ff@ȣ�@�@�x�@�ff@�{@��j@�bN@� �@��;@�l�@�^5@�G�@�+@���@�Z@���@���@��T@�7L@��
@�C�@��@���@���@�ff@���@��@�X@�V@��@���@��@�n�@�V@�{@��T@�@�x�@�X@�&�@��@��@���@��`@�Ĝ@���@���@�J@��H@�+@�S�@��@��@���@��
@��j@�p�@��#@��@�=q@�5?@���@�%@�1@�ȴ@�@���@���@�p�@��@���@��@��T@�bN@�I�@�b@��-@���@�r�@��#@�  @��R@��;@��/@�-@�+@���@�b@�1@��w@�\)@���@�V@�-@��@�`B@�z�@�b@��
@���@�C�@��H@�=q@���@��`@�  @��
@���@�
=@��\@�-@���@��7@�7L@�Ĝ@�A�@���@�33@�ȴ@��+@�~�@�^5@��@��T@��9@��!@�{@� �@���@��@�?}@�p�@���@���@���@��h@���@��/@�1'@���@�\)@�K�@��@��@�/@�~�@��!@�5?@��@��@��@�;d@�"�@�+@�Z@�hs@�5?@�ff@�M�@�G�@�j@��-@���@�=q@�b@���@���@�9X@�V@�G�@��@�r�@�bN@��;@���@�v�@���@�&�@�Ĝ@�  @�+@��@�Z@� �@�  @��@��@���@�C�@��@��y@��H@�"�@�K�@�@��H@�v�@�$�@��T@��h@�p�@�G�@��@��@���@�(�@��
@�"�@�ff@�@��^@��-@���@�O�@��@�%@��/@���@�bN@�9X@��m@���@���@��@�t�@�C�@��@���@�$�@��#@��#@���@�@�{@�$�@�5?@�=q@�G�@v�R@o\)@fff@^{@U�@L�/@C��@<j@5�T@0�@-@)�^@"�@/@�@�@�9@9X@��@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B�BĜB�`BoB-B=qB]/Be`BW
BO�BS�BaHBr�B� B�1B}�Bw�By�B�B�B�hB�JB�bB�VB�hB��B��B��B��B�uB�oB�\B�JB�JB�JB�DB�DB�7B�PB�hB�oB�oB�hB�hB�\B�\B�bB�bB�\B�PB�JB�JB�DB�DB�=B�%B�Bz�B|�Bw�Bq�Bp�BiyB]/BM�B?}B49B+B%�B�B��B�B�BB��BŢB��B�oB� Bu�BjBbNBR�B1'B�B
��B
�mB
��B
ȴB
��B
�%B
YB
B�B
 �B
DB	�B	�fB	�;B	��B	ɺB	ǮB	B	�dB	�LB	�B	��B	w�B	ffB	aHB	\)B	ZB	YB	VB	P�B	I�B	C�B	=qB	33B	�B	bB	1B	B	%B		7B	%B��B	B	B��B�BB�B�)B�B��B��B��B��BȴBĜB�wBB�^B�dB�FB�LB�qB��B�}B�qB�!B�B�B��B��B��B��B��B�7B�B�B�%B�1B�B|�B}�Bz�By�Bu�Bs�Bo�Bn�Bl�Bk�BiyBhsBiyBe`B_;B_;B^5B^5B^5B]/B[#BZBXBW
BT�BQ�BP�BO�BK�BJ�BI�BJ�BF�BA�B?}BA�BD�BD�BC�B2-B1'B/B-B-B/B,B/B9XB0!B#�B%�B'�B+B#�B�B!�B �B'�B�B�B �B!�B�B�B�B�B!�B"�B"�B#�B$�B#�B#�B!�B$�B%�B(�B(�B-B-B-B-B,B,B,B-B49B8RB:^B;dB>wBA�BC�BH�BI�BK�BK�BK�BK�BR�BT�BVBW
BYBe`BhsBiyBhsBk�Bl�Bl�Bo�Bp�Bt�Bt�Bu�Bw�Bx�Bz�B�B�JB��B��B��B��B��B��B�B�-B��B��B��B�#B�5B�BB�ZB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B	B	\B	�B	0!B	?}B	B�B	N�B	^5B	`BB	e`B	jB	iyB	ffB	dZB	dZB	cTB	dZB	dZB	e`B	ffB	gmB	iyB	k�B	k�B	k�B	m�B	n�B	p�B	q�B	p�B	o�B	m�B	m�B	l�B	l�B	n�B	p�B	q�B	p�B	o�B	n�B	n�B	n�B	n�B	o�B	r�B	o�B	hsB	hsB	e`B	cTB	cTB	bNB	e`B	hsB	m�B	ffB	cTB	x�B	y�B	w�B	w�B	u�B	w�B	}�B	� B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�qB	�wB	�dB	�LB	��B	ƨB	��B	ǮB	ǮB	ȴB	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�
B	��B	�B	��B	��B	ȴB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�5B	�;B	�BB	�BB	�HB	�HB	�BB	�BB	�BB	�;B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�ZB	�ZB	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
VB
�B
&�B
+B
2-B
9XB
B�B
K�B
N�B
S�B
ZB
]/B
dZB
iyB
m�B
q�B
v�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B�BƨB�fB{B0!BC�BaHBjB]/BQ�BVBcTBs�B�B�DB�Bz�B{�B�B�7B�{B�\B�oB�hB�uB��B��B��B��B��B��B�oB�VB�PB�PB�PB�VB�PB�hB�{B��B��B�uB�uB�hB�bB�hB�hB�bB�PB�JB�PB�JB�JB�DB�1B�B|�B}�Bx�Br�Bq�Bl�B_;BQ�BC�B7LB-B(�B!�BB�B�ZB�B��B�B��B�B{�Br�BjB\)B7LB�BB
�B
��B
��B
�9B
��B
e`B
N�B
'�B
�B	��B	�B	�sB	�B	��B	ȴB	ĜB	�jB	�XB	�RB	�3B	�B	m�B	ffB	^5B	\)B	[#B	XB	VB	O�B	H�B	B�B	;dB	$�B	�B	
=B	+B	1B	JB		7B	  B	%B	
=B	B�B�;B�BB�#B�
B��B��B��B��BɺBɺBǮB�qB�wB�LB�RB�}BÖB��B�}B�'B�!B�B�B�B��B��B��B�bB�+B�%B�7B�=B�B�B�B}�B~�By�Bv�Br�Bp�Bn�Bm�Bk�Bk�Bl�BjBaHB`BB_;B_;B`BB_;B^5B]/BZBZBZBT�BS�BS�BN�BN�BM�BM�BL�BH�BC�BD�BF�BI�BF�B49B2-B33B/B/B0!B-B1'B;dB2-B%�B'�B)�B-B)�B�B"�B"�B)�B!�B!�B"�B"�B�B �B!�B$�B#�B$�B%�B'�B%�B%�B%�B%�B'�B)�B0!B1'B0!B.B.B.B-B.B/B2-B7LB;dB<jB=qB@�BC�BF�BH�BJ�BL�BK�BL�BL�BS�BVBW
BXBZBgmBiyBjBiyBl�Bm�Bm�Bp�Bq�Bt�Bt�Bv�Bx�By�B{�B�B�=B��B��B��B��B��B��B�B�'B��B��B��B�#B�5B�HB�fB�yB�B�B�B�B�B�B�B�B��B�B�B�B��B��B��B	  B	DB	�B	/B	>wB	A�B	M�B	^5B	`BB	e`B	k�B	jB	gmB	e`B	dZB	dZB	e`B	e`B	ffB	gmB	hsB	jB	l�B	l�B	l�B	n�B	p�B	q�B	r�B	q�B	p�B	n�B	n�B	m�B	m�B	n�B	q�B	r�B	q�B	p�B	o�B	n�B	o�B	o�B	p�B	t�B	s�B	iyB	l�B	hsB	e`B	dZB	bNB	e`B	hsB	s�B	hsB	^5B	x�B	z�B	x�B	x�B	u�B	w�B	}�B	}�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�qB	��B	�jB	�FB	��B	ǮB	��B	ǮB	ǮB	ȴB	��B	��B	�B	�B	�B	�#B	�#B	�B	�)B	�#B	�B	�
B	�B	�B	��B	ɺB	ǮB	ȴB	��B	ɺB	��B	��B	��B	��B	��B	��B	�
B	�/B	�BB	�;B	�BB	�HB	�HB	�NB	�NB	�BB	�HB	�HB	�BB	�HB	�NB	�NB	�ZB	�TB	�ZB	�`B	�fB	�`B	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
VB
�B
&�B
,B
2-B
:^B
C�B
K�B
O�B
S�B
ZB
^5B
dZB
iyB
n�B
q�B
v�B
z�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452032012011014520320120110145203  AO  ARGQ                                                                        20111130143806  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143806  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145203  IP                  G�O�G�O�G�O�                