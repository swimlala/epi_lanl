CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:56Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ?A   AO  20111130144139  20190522121829  1728_5048_063                   2C  D   APEX                            2142                            040306                          846 @��Z��1   @��[�8��@5J=p��
�b�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   AffA@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C�fC  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,�C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D��D� DfD�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dcy�Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvs3DyffD���D�C3D�y�D�� D�� D�  D�p D���D���D��D�I�Dǰ D��D��3Dڀ D�3D��fD�  D�i�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�33@�33A  A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33C� C��C��C��C	��C��C��C��C��C��C� C��C��C��C��C��C!��C#��C%��C'��C)��C+�3C-� C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm� Co��Cq��Cs� Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fD` D� DffD�fDffD��DffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fD` D�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� D` D�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD� DffD��Dl�D�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<��D=l�D=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFl�DF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK� DLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^` D^�fD_ffD_�fD`ffD`�fDaffDa�fDbl�Db�fDc` Dc� DdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDol�Do�fDpffDp�fDq` Dq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvY�DyL�D�� D�6fD�l�D��3D��3D�3D�c3D�� D�� D��D�<�Dǣ3D���D��fD�s3D�fD�ɚD�3D�\�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A��TA��A��A��TA��#A�|�A�"�A��A�{A�%A��A�A��FA���A��A��^A��uA��DA���A�ĜA�oA��DA��^A�ȴA�ȴA��jA��A��hA�VA�%A��#A���A��\A�p�A�I�A�33A�&�A���A���A��RA�VA��A���A��A�\)A��#A��uA�n�A�S�A�E�A�I�A�A�A�E�A�E�A�?}A�9XA��A���A��A��;A�ȴA��9A���A��A�ZA�9XA�oA��HA��jA��\A�z�A�bNA�Q�A� �A�z�A��9A��7A�bNA��A�7LA��A��A�E�A�1'A��
A��7A�  A���A�G�A��-A�\)A��A�ƨA�;dA��9A�S�A�hsA�~�A��FA��yA�A�&�A��-A�1'A��wA��mA�A�5?A�VA�5?A�JA�hsA��A�=qA�7LA�A�A�n�A�;A~�RA}�^A|�Aw%Aux�At�Aq��An��Am��AihsAf��Ae�PAcƨA_�PA\ZAYl�AV��AVM�AVffAV��AW;dAW\)AT�RASARbAQoAO�^AKVAG��AD��A?t�A=��A<��A;�A:�RA8z�A5A4�jA3�A2Q�A21'A2(�A21A133A/G�A,�HA+l�A+oA*�+A)��A(JA'�7A'%A&9XA%K�A$1'A#S�A"ȴA"-A!"�A��A�7A9XA�FA�A=qA7LAffA/A��A��A�`A��A�A�yAffA��A��A  AXA�!A��A�A�A
�A	O�A��AS�A��A-AdZA�A bN@��
@�o@��R@���@��m@�33@�~�@�7L@��!@�7L@�w@�@��@�@�@��@�S�@���@�33@�x�@�!@�p�@߶F@�E�@ܛ�@�$�@�p�@�G�@��@�bN@׍P@�^5@��
@ҟ�@�O�@�A�@���@��@�A�@�l�@�V@�Ĝ@ǝ�@Ɨ�@���@�&�@ě�@�A�@���@�o@¸R@�@�p�@���@�9X@��@�@���@��@��@���@�|�@�"�@��@���@��\@�^5@�E�@�-@��T@���@�G�@���@���@�r�@� �@���@�C�@�@���@�{@���@��T@���@���@�p�@�?}@��9@�Z@��@�\)@�ȴ@���@�`B@�/@��@���@�A�@���@���@�r�@�j@��u@��u@��@��@�n�@�$�@�J@��@���@���@�t�@�-@�A�@�|�@��y@��@���@���@�=q@���@�X@�bN@��
@���@�l�@�K�@�33@��@���@��!@�^5@���@���@��7@�hs@��@��`@���@��9@���@���@���@�bN@�I�@�9X@� �@�1@�1@��@��@��@��m@��;@��;@��
@���@�ƨ@���@��
@�ƨ@�dZ@�;d@�S�@�dZ@�o@��R@�ff@�M�@�$�@��@��@���@��-@���@��h@�x�@�p�@�O�@�?}@�&�@��@�V@��@��@�z�@�r�@�I�@�b@��m@�ƨ@���@�dZ@�o@��!@�v�@�M�@�-@�J@��#@�p�@��@���@��@��9@��@�(�@���@��w@��P@�S�@�33@�o@���@��+@�E�@��@��T@��-@�`B@�`B@�X@�O�@���@���@��@�z�@�Z@�1'@���@�ƨ@���@�K�@�+@�o@��@��R@�v�@�M�@��@���@��7@�G�@��`@���@��j@���@�z�@�A�@�(�@�b@��F@�t�@�K�@�o@���@�E�@�@�@�p�@�&�@���@��@��u@�z�@�bN@�Z@�A�@�(�@���@��w@�t�@�;d@�
=@��@���@���@��^@|1@rn�@jM�@b��@[o@S��@K��@D�j@<1@6�@1��@+t�@&E�@"-@E�@=q@�@�@I�@Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A��A��TA��A��A��TA��#A�|�A�"�A��A�{A�%A��A�A��FA���A��A��^A��uA��DA���A�ĜA�oA��DA��^A�ȴA�ȴA��jA��A��hA�VA�%A��#A���A��\A�p�A�I�A�33A�&�A���A���A��RA�VA��A���A��A�\)A��#A��uA�n�A�S�A�E�A�I�A�A�A�E�A�E�A�?}A�9XA��A���A��A��;A�ȴA��9A���A��A�ZA�9XA�oA��HA��jA��\A�z�A�bNA�Q�A� �A�z�A��9A��7A�bNA��A�7LA��A��A�E�A�1'A��
A��7A�  A���A�G�A��-A�\)A��A�ƨA�;dA��9A�S�A�hsA�~�A��FA��yA�A�&�A��-A�1'A��wA��mA�A�5?A�VA�5?A�JA�hsA��A�=qA�7LA�A�A�n�A�;A~�RA}�^A|�Aw%Aux�At�Aq��An��Am��AihsAf��Ae�PAcƨA_�PA\ZAYl�AV��AVM�AVffAV��AW;dAW\)AT�RASARbAQoAO�^AKVAG��AD��A?t�A=��A<��A;�A:�RA8z�A5A4�jA3�A2Q�A21'A2(�A21A133A/G�A,�HA+l�A+oA*�+A)��A(JA'�7A'%A&9XA%K�A$1'A#S�A"ȴA"-A!"�A��A�7A9XA�FA�A=qA7LAffA/A��A��A�`A��A�A�yAffA��A��A  AXA�!A��A�A�A
�A	O�A��AS�A��A-AdZA�A bN@��
@�o@��R@���@��m@�33@�~�@�7L@��!@�7L@�w@�@��@�@�@��@�S�@���@�33@�x�@�!@�p�@߶F@�E�@ܛ�@�$�@�p�@�G�@��@�bN@׍P@�^5@��
@ҟ�@�O�@�A�@���@��@�A�@�l�@�V@�Ĝ@ǝ�@Ɨ�@���@�&�@ě�@�A�@���@�o@¸R@�@�p�@���@�9X@��@�@���@��@��@���@�|�@�"�@��@���@��\@�^5@�E�@�-@��T@���@�G�@���@���@�r�@� �@���@�C�@�@���@�{@���@��T@���@���@�p�@�?}@��9@�Z@��@�\)@�ȴ@���@�`B@�/@��@���@�A�@���@���@�r�@�j@��u@��u@��@��@�n�@�$�@�J@��@���@���@�t�@�-@�A�@�|�@��y@��@���@���@�=q@���@�X@�bN@��
@���@�l�@�K�@�33@��@���@��!@�^5@���@���@��7@�hs@��@��`@���@��9@���@���@���@�bN@�I�@�9X@� �@�1@�1@��@��@��@��m@��;@��;@��
@���@�ƨ@���@��
@�ƨ@�dZ@�;d@�S�@�dZ@�o@��R@�ff@�M�@�$�@��@��@���@��-@���@��h@�x�@�p�@�O�@�?}@�&�@��@�V@��@��@�z�@�r�@�I�@�b@��m@�ƨ@���@�dZ@�o@��!@�v�@�M�@�-@�J@��#@�p�@��@���@��@��9@��@�(�@���@��w@��P@�S�@�33@�o@���@��+@�E�@��@��T@��-@�`B@�`B@�X@�O�@���@���@��@�z�@�Z@�1'@���@�ƨ@���@�K�@�+@�o@��@��R@�v�@�M�@��@���@��7@�G�@��`@���@��j@���@�z�@�A�@�(�@�b@��F@�t�@�K�@�o@���@�E�@�@�@�p�@�&�@���@��@��u@�z�@�bN@�Z@�A�@�(�@���@��w@�t�@�;d@�
=@��@���@���@��^@|1@rn�@jM�@b��@[o@S��@K��@D�j@<1@6�@1��@+t�@&E�@"-@E�@=q@�@�@I�@Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B�B�9B�dB�qB��BɺB��BɺBÖB��B�jB��B��B��B��B��B�B�wB�B�NB�`B�fB�fB�`B�TB�/B�B�B�#B�#B�
B�B��BBbBA�BT�BYBjBp�Bq�Bz�B�B��B��B��B��B��B�B�!B�-B�?B�FB�XB�dB�dB�dB�dB�wB�}B��BBĜBǮB��B��B��B��B�B�TB�`B�;B�B�B�'B'�BɺB��B�B��B#�B�B!�B%�B�B{B{B�B�B!�B\B�B�`B�/B�/BɺB�dB��B��B�bBx�Be`BbNBN�B49B  B
ȴB
��B
�PB
�B
s�B
e`B
B�B
'�B
�B
VB
B	�B	�B	��B	��B	�LB	��B	��B	� B	y�B	dZB	]/B	.B		7B	B��B	B	
=B	�B	-B	1'B	-B	1'B	6FB	8RB	?}B	VB�fBÖB�-B��B��B��B�hB�uB�7B�7B�%B�%B�%B�B�B�+B�1B�B�B�B�B�B�7B�B�B�PB�+B�+B�%B�=B�7B�B�%B�B�B~�B~�Bx�Bt�Bq�Bm�BjBp�Bt�Bt�Bs�Bn�Bl�Bm�Bn�Bo�BjBgmBhsB`BBZBR�BL�BK�BC�B@�BE�BC�BK�B>wB=qB:^B9XB:^B9XB9XB9XB8RB7LB8RB9XB6FB5?B5?B5?B>wB7LB6FB1'B2-B33B8RB:^B5?B1'B33B33B33B5?B5?B7LB49B6FB7LB6FB6FB:^B9XB9XB;dB=qB@�B@�BC�BE�BE�BF�BG�BH�BK�BM�BQ�BS�BXBYBW
BXBYB\)B^5B`BBbNBe`BffBffBgmBgmBhsBhsBjBjBl�Bl�Bn�Bo�Bs�Bu�Bv�Bx�By�B~�B�B�B�B�B�B�%B�=B�VB�hB�{B��B��B��B��B�B�!B�?B�dB�RB�LB�RB�dB�wBĜBȴBɺB��B��B��B��B��B��B�B�B�B�B�/B�BB�TB�fB�sB�B�B�B�B�B�B��B��B��B��B	B	+B		7B	DB	\B	�B	�B	�B	#�B	(�B	-B	0!B	2-B	7LB	:^B	<jB	=qB	@�B	D�B	F�B	J�B	M�B	N�B	N�B	O�B	P�B	S�B	T�B	YB	\)B	^5B	`BB	e`B	hsB	n�B	v�B	y�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�JB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�FB	�XB	�^B	�jB	�jB	�qB	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
bB
�B
 �B
(�B
.B
6FB
<jB
G�B
K�B
P�B
W
B
\)B
aHB
e`B
hsB
m�B
r�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B�B�FB�qB�qB��BɺB��B��BÖBB�wB��B��B��B��B��B�B�jB�
B�NB�`B�fB�fB�fB�ZB�;B�
B�B�)B�)B�B�#B��BBhBB�BW
B[#Bl�Br�Br�B}�B�%B��B��B��B��B��B�B�!B�-B�?B�LB�^B�dB�jB�jB�jB�}B��BBÖBŢBȴB��B��B��B��B�B�ZB�sB�TB�;B�)BŢB9XB��B��B�B��B-B$�B#�B'�B�B�B�B�B!�B)�B�B�B�B�NB�HB��B��B�B��B��B� BiyBgmBXB?}B{B
��B
��B
�bB
�7B
z�B
q�B
M�B
,B
�B
oB
+B
B	�5B	�B	��B	�qB	�B	��B	�%B	|�B	iyB	ffB	6FB	bB	DB��B	B		7B	�B	-B	8RB	2-B	49B	9XB	=qB	M�B	�B�B�B�RB��B��B��B��B��B�PB�\B�7B�+B�+B�%B�+B�VB�hB�1B�+B�B�1B�1B�=B�B�B�\B�=B�7B�1B�JB�JB�1B�7B�1B�%B�B�B{�Bw�Bu�Br�Bl�Bq�Bu�Bw�Bw�Bp�Bo�Bq�Bq�Br�Bm�BjBl�BdZB`BBW
BO�BP�BI�BE�BI�BI�BQ�B@�B?}B;dB<jB>wB;dB;dB<jB=qB:^B;dB;dB7LB6FB8RB:^BD�B;dB9XB49B6FB5?B;dB<jB8RB5?B49B49B49B6FB7LB9XB8RB8RB9XB8RB:^B<jB:^B;dB=qB@�BB�BB�BE�BG�BF�BG�BH�BJ�BL�BO�BQ�BS�BZB\)BYBXB[#B^5B`BBaHBcTBffBgmBgmBhsBhsBhsBiyBk�Bk�Bl�Bm�Bo�Bp�Bu�Bv�Bw�By�Bz�B� B�B�B�B�B�B�+B�=B�\B�uB��B��B��B��B�B�B�-B�RB�dB�XB�LB�RB�dB�wBŢB��B��B��B��B��B��B��B�B�B�B�B�)B�BB�NB�ZB�mB�yB�B�B�B�B�B�B��B��B��B	  B	B	1B		7B	JB	bB	�B	�B	�B	#�B	(�B	-B	1'B	2-B	7LB	:^B	<jB	=qB	@�B	D�B	F�B	J�B	M�B	N�B	N�B	O�B	P�B	S�B	T�B	YB	]/B	^5B	`BB	e`B	iyB	o�B	w�B	y�B	{�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�+B	�+B	�7B	�DB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�9B	�LB	�^B	�dB	�qB	�qB	�qB	�}B	��B	B	ĜB	ĜB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�/B	�;B	�;B	�;B	�BB	�BB	�NB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
bB
�B
 �B
(�B
.B
6FB
=qB
G�B
L�B
Q�B
XB
\)B
aHB
e`B
hsB
m�B
s�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<��
<D��<#�
<#�
<#�
<#�
<D��<#�
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
<e`B<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452152012011014521520120110145215  AO  ARGQ                                                                        20111130144139  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144139  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145215  IP                  G�O�G�O�G�O�                