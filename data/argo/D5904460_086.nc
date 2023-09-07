CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-21T20:16:17Z AOML 3.0 creation; 2016-08-07T21:17:42Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20151121201617  20160807141743  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  5285_8895_086                   2C  D   APEX                            6487                            072314                          846 @׀[X�31   @׀[��W�@.��+J�c���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   B   B   @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B"  B(��B0ffB6  B@  BH  BP  BXffB^ffBh  Bp  Bx  B�33B�ffB���B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D�3D�9�D�p D��3D�fD�FfD�� D��fD�fD�33D�vfDǼ�Dͩ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@a�@�(�@�(�A
{A+�AJ{Aj{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B
�B�B�B$�B+Q�B2�B8�BB�BJ�BR�BZ�B`�Bj�Br�Bz�B�u�B���B�\B�B�B�B�B�B�B�B�B�B�B�u�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�Bݨ�B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�C �HC�HC�HC�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�]qC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�]qC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD.�D�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!��D"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt�RDt��Dy�RD�'\D�M�D��)D��\D�*�D�Z�D��)D��D��D�G\D���D���Dͽ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�dZA�ffA�dZA�dZA�ffA�bNA�bNA�VA�K�A�(�A�
=A�
=A�1A�A���A��TA��A���AڃA٣�A��A�|�A�|�A���A�A��DA�7LA�n�A�n�A�/A�
=A�1A�Q�A��uA�{A��PA��^A�v�A��A�K�A{S�Aw�Au%Ar��Ap�jAm?}AjI�AfE�AdffA`ZA]�#A\r�A[
=AWO�AQ�FAM�AEx�AB{A?�hA=��A:�+A61A3�A0�A/t�A-
=A+��A)hsA'�A&(�A$��A#�PA"��A"I�A"JA" �A"n�A �A�wA�PAƨA�A�#AhsA��A�`AQ�AdZA�FA�RA~�Az�AhsA�A��A��A�!A�wA/A�!A��AbNA�A�A��A�;A�FA
��A	�^A	dZA�AA�A&�A��A{A�A�wA�mA&�A��A	XA	��A
$�A
bNAO�A33A
��A
�RA
 �A	�7A	O�A	%A�A�A\)AĜAjA{A��Al�A�AA�HA�!A�!A��A^5A �A�A��AdZAXAC�AVA��A��A~�AZA�AA�A�A��Ax�AG�A+A�AVAA �`A �!A �+A jA �+Al�A=qA~�Az�AjAE�A{A��A�-A�PAl�AO�A&�A ��A ȴA �A -@��F@�@�@��!@���@�;d@�{@�%@�Z@�ƨ@��\@���@��`@�1'@�l�@�@�^@�O�@�V@��@�Q�@�9X@��
@�dZ@��T@�@�z�@�l�@�n�@�5?@��@�/@�j@�;d@�~�@�V@��@���@�bN@� �@� �@�@�(�@�  @��
@�w@㝲@�;d@�+@�-@�bN@�t�@�"�@��@�@���@�@�-@݁@� �@��@�"�@ڟ�@�5?@�=q@�@ف@���@�r�@��m@�|�@�S�@�@�v�@��@ՙ�@���@�  @�ƨ@���@���@ӥ�@�dZ@�o@ҏ\@Ѳ-@ёh@с@�7L@У�@���@���@�=q@��@ͩ�@�X@�O�@�V@̃@�Q�@� �@�(�@ˮ@�@�ff@��@ȋD@�ƨ@�|�@Ƈ+@�J@ź^@Ł@�%@��/@�9X@å�@å�@�;d@�@��@��w@�dZ@���@��\@�ff@�hs@���@�bN@�1'@��@��F@��P@�;d@�ff@��@��`@��9@���@���@�Z@�1@��F@�t�@��@��!@���@��+@�V@�5?@��@��@��T@��T@��T@���@���@�X@���@���@�r�@�A�@�  @���@�\)@�\)@�S�@�;d@���@�^5@��@���@��#@���@�/@���@���@�Ĝ@�b@��m@��@�S�@�S�@�|�@�C�@��\@��!@�$�@��@�@���@�p�@��`@���@�dZ@�S�@�+@��@��@��+@�5?@�@�hs@��u@��@��w@�K�@�ȴ@�v�@��T@�X@�&�@�Ĝ@�Z@�1@���@��@��\@�5?@�$�@�J@���@�hs@�?}@���@���@�j@�  @��F@�\)@�33@���@���@�5?@��^@��h@���@���@���@��7@�p�@�`B@���@��@�r�@���@��@�|�@�l�@�\)@�"�@���@�v�@�$�@��#@��-@�x�@�O�@��@�z�@�I�@��@��m@���@�t�@�dZ@�"�@���@��y@���@�=q@�@���@��^@�?}@�V@���@��/@��@�z�@�(�@��@��P@�;d@��@��R@��+@��@���@��h@�hs@�G�@��@���@��u@�Q�@��w@��@��@��-@��j@w\)@n��@b�@W��@K�
@A&�@:~�@5��@/+@(1'@"��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A�dZA�dZA�ffA�dZA�dZA�ffA�bNA�bNA�VA�K�A�(�A�
=A�
=A�1A�A���A��TA��A���AڃA٣�A��A�|�A�|�A���A�A��DA�7LA�n�A�n�A�/A�
=A�1A�Q�A��uA�{A��PA��^A�v�A��A�K�A{S�Aw�Au%Ar��Ap�jAm?}AjI�AfE�AdffA`ZA]�#A\r�A[
=AWO�AQ�FAM�AEx�AB{A?�hA=��A:�+A61A3�A0�A/t�A-
=A+��A)hsA'�A&(�A$��A#�PA"��A"I�A"JA" �A"n�A �A�wA�PAƨA�A�#AhsA��A�`AQ�AdZA�FA�RA~�Az�AhsA�A��A��A�!A�wA/A�!A��AbNA�A�A��A�;A�FA
��A	�^A	dZA�AA�A&�A��A{A�A�wA�mA&�A��A	XA	��A
$�A
bNAO�A33A
��A
�RA
 �A	�7A	O�A	%A�A�A\)AĜAjA{A��Al�A�AA�HA�!A�!A��A^5A �A�A��AdZAXAC�AVA��A��A~�AZA�AA�A�A��Ax�AG�A+A�AVAA �`A �!A �+A jA �+Al�A=qA~�Az�AjAE�A{A��A�-A�PAl�AO�A&�A ��A ȴA �A -@��F@�@�@��!@���@�;d@�{@�%@�Z@�ƨ@��\@���@��`@�1'@�l�@�@�^@�O�@�V@��@�Q�@�9X@��
@�dZ@��T@�@�z�@�l�@�n�@�5?@��@�/@�j@�;d@�~�@�V@��@���@�bN@� �@� �@�@�(�@�  @��
@�w@㝲@�;d@�+@�-@�bN@�t�@�"�@��@�@���@�@�-@݁@� �@��@�"�@ڟ�@�5?@�=q@�@ف@���@�r�@��m@�|�@�S�@�@�v�@��@ՙ�@���@�  @�ƨ@���@���@ӥ�@�dZ@�o@ҏ\@Ѳ-@ёh@с@�7L@У�@���@���@�=q@��@ͩ�@�X@�O�@�V@̃@�Q�@� �@�(�@ˮ@�@�ff@��@ȋD@�ƨ@�|�@Ƈ+@�J@ź^@Ł@�%@��/@�9X@å�@å�@�;d@�@��@��w@�dZ@���@��\@�ff@�hs@���@�bN@�1'@��@��F@��P@�;d@�ff@��@��`@��9@���@���@�Z@�1@��F@�t�@��@��!@���@��+@�V@�5?@��@��@��T@��T@��T@���@���@�X@���@���@�r�@�A�@�  @���@�\)@�\)@�S�@�;d@���@�^5@��@���@��#@���@�/@���@���@�Ĝ@�b@��m@��@�S�@�S�@�|�@�C�@��\@��!@�$�@��@�@���@�p�@��`@���@�dZ@�S�@�+@��@��@��+@�5?@�@�hs@��u@��@��w@�K�@�ȴ@�v�@��T@�X@�&�@�Ĝ@�Z@�1@���@��@��\@�5?@�$�@�J@���@�hs@�?}@���@���@�j@�  @��F@�\)@�33@���@���@�5?@��^@��h@���@���@���@��7@�p�@�`B@���@��@�r�@���@��@�|�@�l�@�\)@�"�@���@�v�@�$�@��#@��-@�x�@�O�@��@�z�@�I�@��@��m@���@�t�@�dZ@�"�@���@��y@���@�=q@�@���@��^@�?}@�V@���@��/@��@�z�@�(�@��@��P@�;d@��@��R@��+@��@���@��h@�hs@�G�@��@���@��u@�Q�@��wG�O�@��@��-@��j@w\)@n��@b�@W��@K�
@A&�@:~�@5��@/+@(1'@"��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBcTBcTBcTBdZBdZBdZBdZBe`BffBiyBq�Bx�B� B�%B�7B�PB�\B�oB��B�sB�jB	}�B
!�B
'�B	�B	�RB	ɺB	�B	�mB	�B	ƨB	�B	��B	��B	�hB	�B	gmB	^5B	]/B	L�B	[#B	aHB	bNB	`BB	]/B	[#B	W
B	W
B	P�B	YB	`BB	^5B	[#B	[#B	`BB	[#B	N�B	N�B	?}B	-B	{B��B�B�BB�
B��BŢB�qB�LB�?B�?B�FB�?B�?B�FB�}B��BǮBŢBƨBĜBÖB�jBĜB�`B	�B	oB	\B	
=B		7B	\B	%�B	2-B	C�B	E�B	N�B	P�B	-B	33B	5?B	C�B	K�B	K�B	H�B	>wB	7LB	49B	/B	0!B	0!B	0!B	0!B	1'B	2-B	33B	7LB	K�B	S�B	o�B	�VB	��B	�-B	��B	ŢB	�B	�;B	�5B	�5B	�NB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
\B
�B
%�B
%�B
%�B
$�B
#�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
\B
VB
VB
VB
VB
PB
PB
PB
PB
PB
PB
JB
JB
DB
	7B
+B
%B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
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
B
B
B
B
%B
+B
+B
+B
+B
%B
%B
%B
%B
%B
B
%B
%B
%B
%B
+B
	7B
	7B
	7B
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
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
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
"�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
$�B
'�B
(�B
,B
33B
6FB
;dB
C�B
H�B
K�B
Q�B
W
B
]/B
aHB
e`B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111Bc$Bc$Bc(Bc$Bd)Bd)Bd*Bd*Be/Bf9BiIBqzBx�B�B��B�B�"B�+B�=B��B�BB�OB	}�B
!�B
'�B	�ZB	�&B	ɑB	�zB	�@B	��B	�tB	��B	��B	�_B	�:B	��B	g=B	^B	\�B	L�B	Z�B	aB	bB	`B	\�B	Z�B	V�B	V�B	P�B	X�B	`B	^ B	Z�B	Z�B	`
B	Z�B	N�B	N�B	?CB	,�B	DB��B�`B�B��BːB�kB�<B�B�B�
B�B�	B�
B�B�FBˑB�vB�iB�pB�dB�]B�3B�eB�%B	EB	3B	B	
B	�B	B	%�B	1�B	CWB	EdB	N�B	P�B	,�B	2�B	5B	CXB	K�B	K�B	HtB	>7B	7B	3�B	.�B	/�B	/�B	/�B	/�B	0�B	1�B	2�B	7B	K�B	S�B	o]B	�B	�^B	��B	�DB	�_B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�"B	� B	�!B	�B	�)B	�)B	�(B	�(B	�4B	�3B	�3B	�8B	�8B	�:B	�6B	�>B	�9B	�8B	�AB	�?B	�AB	�>B	�@B	�QB	�^B	�hB	�hB	�qB	�wB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
pB
%�B
%�B
%�B
$�B
#�B
"�B
"�B
"�B
"�B
"�B
!�B
 |B
xB
lB
iB
XB
TB
^B
VB
GB
7B
4B
/B
'B
 B
B
B
B
B
B
B
B
B
B
B
	B
B
B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�~B	��B	��B	��B	��B	��B	��B	�kB	�tB	�tB	�tB	�nB	�`B	�`B	�`B	�`B	�aB	�fB	�fB	�fB	�aB	�mB	�uB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�B	�xB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
B
�B
B
B
	B
	B

B
B

B
B
B
B
B
B
B
B
#B
"B
"B
*B
)B
*B
,B
0B
/B
4B
;B
=B
:B
:B
9B
AB
AB
BB
BB
@B
HB
IB
NB
QB
NB
OB
RB
\B
ZB
[B
\B
[B
ZB
[B
\B
]B
aB
_B
_B
eB
fB
fB
cB
eB
gB
eB
eB
lB
lB
lB
jB
iB
sB
qB
rB
qB
 xB
 xB
 yB
 wB
!}B
!B
!~B
!~B
"�B
!B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!~B
!~B
!B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
$�G�O�B
(�B
+�B
2�B
5�B
;B
CJB
HfB
KzB
Q�B
V�B
\�B
`�B
eB
l?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.63 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417432016080714174320160807141743  AO  ARCAADJP                                                                    20151121201617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151121201617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151121201617  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141743  IP                  G�O�G�O�G�O�                