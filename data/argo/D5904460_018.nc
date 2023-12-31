CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:56Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221256  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_018                   2C  D   APEX                            6487                            072314                          846 @�'Bi�1   @�'CO��@+��t��c�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH��BO33BX  B`  BhffBp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D�  D�I�D�� D��3D���D�<�D�p D��3D��D�@ D�vfDǬ�D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)A
{A*{AJ{Aj{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A��
B�B
�B�B�B"�B*�B2�B:�BB�BKQ�BQ�RBZ�Bb�Bj�Br�Bz�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �HC��C��C�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh��Cj��Cl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-.�D-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO.�DO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt��Dy��D�)D�]�D��)D��\D��D�P�D��)D��\D� �D�T)D���D���D�`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�bNA�`BA�^5A�^5A�bNA�bNA�M�A�I�A�7LA�-A��A�VA�
=A�A�  A���A���A���A���A���A��A��yA��A�1'AɁA�^5A��PA�v�A��A�|�A��!A���A�l�A�JA��#A�&�A��FA�C�A�7LA� �A���A�1'A���A��A�M�A��A���A��A��A�1A��A�-A��+A�t�A��A�JA�dZA��A���A�ffA�&�A��wA��7A��9A�A�A��A�ffA���A��A�G�A��+A��A�VA�l�A��A�ƨA�Q�A{+Aw�Au��At{Aq7LAl�Af��Ac�FAaVA]�#A[�;AV�DASl�AQƨAO�wALbNAH-AF�`AC�A>�uA<�A;"�A9S�A7t�A6E�A4�\A1��A.�A,VA+`BA*�jA)��A(�A(�A'?}A&�/A'�7A)l�A+�^A+�mA+|�A)t�A%��A#?}A!/A��An�AbAXA��A��A�A�A�A  AZAx�AjA��A1'A��AC�AI�A��A|�A�-A~�Ap�Az�A��A�\A��A
=A�hA{A^5A�A
�A	l�A��A�AdZA\)Av�A��A��A�PA ��A b@��@��y@�p�@��@���@��u@���@��9@�1@��j@��;@��@���@�C�@�-@��@�R@��@�(�@��j@�J@�z�@�ȴ@��@�S�@�n�@홚@��/@�j@�l�@��@���@���@��@�@��@�\@��H@�V@�X@�b@�K�@�-@�R@�"�@�j@�u@��@�5?@�5?@�M�@�R@�R@�v�@�?}@���@��@�o@�-@�X@��@��@�j@ߥ�@�K�@��@�ȴ@���@�l�@�K�@�M�@݁@ܛ�@ܬ@�9X@��;@ۥ�@ۥ�@�S�@ڟ�@�$�@��@��#@�X@�O�@�%@؛�@�Q�@��@ץ�@��@֧�@�V@��@��#@���@պ^@�hs@��@�Q�@�;d@�ȴ@�~�@�5?@��@ѡ�@�/@���@�Ĝ@�A�@϶F@υ@�K�@�
=@Ώ\@��@��@�bN@�ƨ@�t�@�;d@�@�E�@��@ɩ�@�p�@�?}@�%@�z�@��@��m@Ǿw@ǍP@�;d@���@�$�@���@�hs@���@�bN@�1@��
@öF@�dZ@��@�@��@¸R@�5?@��@��D@���@��!@�M�@���@���@�bN@��@�o@���@��+@�ff@�$�@��#@���@��@�O�@���@�z�@�I�@��w@�;d@��@���@�5?@�@�x�@�?}@�&�@���@���@�9X@�ƨ@��P@�+@���@�v�@���@�p�@�&�@���@���@��j@��@�A�@�9X@���@���@�l�@�
=@���@��@��7@�`B@�X@��@�1@�33@���@�=q@��#@�7L@��j@��@�I�@��
@�dZ@�33@��H@���@��+@�{@���@��@�7L@��/@�I�@�(�@��@�dZ@���@��@��-@�`B@�G�@�7L@��/@�1'@��@���@�;d@�ȴ@���@��+@�n�@�5?@���@�hs@��@���@���@�z�@�Z@�(�@��;@�K�@�@��@�ȴ@��+@�n�@�E�@�@��h@���@��u@�  @��;@�ƨ@���@�C�@�@��H@���@���@�V@��@��T@��^@�p�@�G�@�?}@�/@�%@���@�bN@�9X@��m@�|�@��@���@�~�@�V@�{@�x�@�G�@�7L@�7L@�V@��@���@�Z@��;@���@�dZ@�33@��@���@�^5@�$�@�@��#@���@���@���@���@�S�@���@��R@��+@�V@��/@��F@|j@q�7@h1'@b=q@Z�@R^5@I7L@B��@;��@5�@0Q�@(�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�dZA�bNA�`BA�^5A�^5A�bNA�bNA�M�A�I�A�7LA�-A��A�VA�
=A�A�  A���A���A���A���A���A��A��yA��A�1'AɁA�^5A��PA�v�A��A�|�A��!A���A�l�A�JA��#A�&�A��FA�C�A�7LA� �A���A�1'A���A��A�M�A��A���A��A��A�1A��A�-A��+A�t�A��A�JA�dZA��A���A�ffA�&�A��wA��7A��9A�A�A��A�ffA���A��A�G�A��+A��A�VA�l�A��A�ƨA�Q�A{+Aw�Au��At{Aq7LAl�Af��Ac�FAaVA]�#A[�;AV�DASl�AQƨAO�wALbNAH-AF�`AC�A>�uA<�A;"�A9S�A7t�A6E�A4�\A1��A.�A,VA+`BA*�jA)��A(�A(�A'?}A&�/A'�7A)l�A+�^A+�mA+|�A)t�A%��A#?}A!/A��An�AbAXA��A��A�A�A�A  AZAx�AjA��A1'A��AC�AI�A��A|�A�-A~�Ap�Az�A��A�\A��A
=A�hA{A^5A�A
�A	l�A��A�AdZA\)Av�A��A��A�PA ��A b@��@��y@�p�@��@���@��u@���@��9@�1@��j@��;@��@���@�C�@�-@��@�R@��@�(�@��j@�J@�z�@�ȴ@��@�S�@�n�@홚@��/@�j@�l�@��@���@���@��@�@��@�\@��H@�V@�X@�b@�K�@�-@�R@�"�@�j@�u@��@�5?@�5?@�M�@�R@�R@�v�@�?}@���@��@�o@�-@�X@��@��@�j@ߥ�@�K�@��@�ȴ@���@�l�@�K�@�M�@݁@ܛ�@ܬ@�9X@��;@ۥ�@ۥ�@�S�@ڟ�@�$�@��@��#@�X@�O�@�%@؛�@�Q�@��@ץ�@��@֧�@�V@��@��#@���@պ^@�hs@��@�Q�@�;d@�ȴ@�~�@�5?@��@ѡ�@�/@���@�Ĝ@�A�@϶F@υ@�K�@�
=@Ώ\@��@��@�bN@�ƨ@�t�@�;d@�@�E�@��@ɩ�@�p�@�?}@�%@�z�@��@��m@Ǿw@ǍP@�;d@���@�$�@���@�hs@���@�bN@�1@��
@öF@�dZ@��@�@��@¸R@�5?@��@��D@���@��!@�M�@���@���@�bN@��@�o@���@��+@�ff@�$�@��#@���@��@�O�@���@�z�@�I�@��w@�;d@��@���@�5?@�@�x�@�?}@�&�@���@���@�9X@�ƨ@��P@�+@���@�v�@���@�p�@�&�@���@���@��j@��@�A�@�9X@���@���@�l�@�
=@���@��@��7@�`B@�X@��@�1@�33@���@�=q@��#@�7L@��j@��@�I�@��
@�dZ@�33@��H@���@��+@�{@���@��@�7L@��/@�I�@�(�@��@�dZ@���@��@��-@�`B@�G�@�7L@��/@�1'@��@���@�;d@�ȴ@���@��+@�n�@�5?@���@�hs@��@���@���@�z�@�Z@�(�@��;@�K�@�@��@�ȴ@��+@�n�@�E�@�@��h@���@��u@�  @��;@�ƨ@���@�C�@�@��H@���@���@�V@��@��T@��^@�p�@�G�@�?}@�/@�%@���@�bN@�9X@��m@�|�@��@���@�~�@�V@�{@�x�@�G�@�7L@�7L@�V@��@���@�Z@��;@���@�dZ@�33@��@���@�^5@�$�@�@��#@���@���@���@���@�S�@���@��R@��+G�O�@��/@��F@|j@q�7@h1'@b=q@Z�@R^5@I7L@B��@;��@5�@0Q�@(�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BH�BH�BH�BH�BH�BH�BH�BG�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BH�BH�BK�BO�B�VB��B�{B�{B�oB��B��B�XB��BB�RB��B��B�yB'�BG�BE�BJ�B�B��B�7Bs�Bn�BjBe`BW
Be`BiyBt�Bo�BXB.B�B�BƨB�9B�{Bx�BcTBA�B�B  B1BPB
��B
�B
�^B
��B
�{B
�B
o�B
[#B
D�B
�B
+B	��B	�B	��B	�-B	�bB	|�B	k�B	ZB	I�B	1'B	!�B	�B	PB��B�B�sB�#B��BɺBŢB��B�jB�XB�?B�'B�B�!B�'B�'B�-B�-B�?B�jBƨB�fB	 �B	ZB	bNB	e`B	XB	@�B	<jB	<jB	=qB	9XB	8RB	8RB	:^B	:^B	>wB	G�B	O�B	P�B	K�B	G�B	G�B	F�B	C�B	A�B	?}B	=qB	=qB	=qB	H�B	D�B	?}B	8RB	2-B	-B	(�B	(�B	@�B	P�B	XB	R�B	O�B	I�B	B�B	B�B	D�B	M�B	I�B	;dB	8RB	A�B	E�B	D�B	I�B	L�B	P�B	O�B	VB	]/B	bNB	bNB	`BB	]/B	dZB	e`B	n�B	y�B	��B	��B	�bB	�VB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�?B	�jB	�^B	�FB	�3B	�'B	�B	�FB	�XB	�'B	�!B	�?B	ĜB	ĜB	ƨB	��B	��B	��B	��B	ɺB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�B	�
B	�B	�)B	�/B	�/B	�/B	�5B	�;B	�5B	�5B	�;B	�BB	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
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
B
B
B
+B
+B
1B
1B
1B
1B
	7B

=B
	7B
	7B
	7B
	7B
1B
1B
1B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
	7B

=B

=B
DB

=B

=B
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
\B
bB
hB
uB
{B
{B
{B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
/B
9XB
?}B
F�B
I�B
N�B
R�B
YB
]/B
aHB
e`B
gmB
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  BH�BH�BH�BH�BH�BH�BH�BH�BG|BG}BG}BGBGBG|BG~BG}BH�BH�BH�BH�BH�BH�BH�BK�BO�B�!B�cB�GB�GB�9B�dB��B�%BΤB�YB�B��B�lB�AB'�BGyBElBJ�B��B�MB��Bs|Bn_BjHBe&BV�Be%Bi>Bt�BobBW�B-�BCB��B�kB��B�@Bx�BcBALBnB
��B�BB
��B
��B
�#B
��B
�>B
��B
ocB
Z�B
DcB
}B
�B	��B	�NB	ҼB	��B	�,B	|�B	kOB	Y�B	I�B	0�B	!�B	\B	B��B�jB�?B��BϫBɇB�oB�QB�7B�"B�B��B��B��B��B��B��B��B�B�2B�rB�,B	 �B	Y�B	bB	e$B	W�B	@HB	<.B	</B	=6B	9B	8B	8B	:!B	:"B	>9B	GqB	O�B	P�B	K�B	GpB	GqB	FjB	CWB	AJB	?@B	=5B	=1B	=3B	HtB	D`B	?@B	8B	1�B	,�B	(�B	(�B	@EB	P�B	W�B	R�B	O�B	I|B	BQB	BOB	DZB	M�B	IyB	;%B	8B	AIB	EaB	D\B	IyB	L�B	P�B	O�B	U�B	\�B	b
B	bB	` B	\�B	dB	eB	nVB	y�B	�PB	�PB	�B	�B	��B	��B	�sB	�UB	�AB	�[B	�rB	�lB	�aB	�hB	��B	�yB	�gB	�[B	�gB	�xB	��B	��B	��B	�$B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�UB	�WB	�bB	͌B	ΔB	ϗB	̅B	�uB	�aB	�sB	̄B	̃B	�B	ΓB	͋B	ΔB	ОB	ΑB	ҭB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�$B	�,B	�)B	�,B	�2B	�.B	�6B	�4B	�>B	�4B	�=B	�GB	�KB	�QB	�NB	�OB	�IB	�FB	�JB	�KB	�GB	�HB	�HB	�GB	�JB	�IB	�CB	�AB	�CB	�BB	�EB	�DB	�EB	�CB	�FB	�GB	�HB	�NB	�NB	�QB	�\B	�ZB	�ZB	�\B	�[B	�bB	�fB	�lB	�lB	�tB	�xB	�B	�~B	�~B	�B	�B	�~B	��B	�wB	�xB	�xB	�yB	�lB	�_B	�fB	�bB	�bB	�cB	�mB	�mB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
	�B
	�B

�B
B
�B
�B
B
B
B
B
B
B
B
B
B
)B
1B
+B
-B
7B
-B
.B
.B
4B
7B
<B
@B
AB
?B
@B
@B
=B
>B
?B
IB
IB
OB
NB
MB
MB
SB
RB
SB
RB
RB
[B
ZB
\B
[B
ZB
bB
bB
`B
`B
_B
`B
]B
gB
eB
lB
lB
jB
jB
iB
qB
 xB
 {B
tB
qB
qB
 yB
 xB
!~B
 xB
 xB
 yB
 vB
!}B
!|B
!}B
!|B
!~B
!}B
!}B
!B
!~B
"�B
"�B
"�B
"�G�O�B
$�B
.�B
9	B
?.B
FXB
IkB
N�B
R�B
X�B
\�B
`�B
eB
g B
nJ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.63 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221256    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221256  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221256  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                