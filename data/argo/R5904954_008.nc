CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       RH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191649  20181005191649  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ע��1   @ע���@2�Q���c�+I�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@�CB  CD  CF  CG�fCJ  CL  CN  CO�fCQ�fCS�fCV  CX  CZ  C[�fC^  C`  Ca�fCc�fCe�fCg�fCj  Cl  Cm�fCp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C��C��C��3C�  C��C��C��3C��3C�  C��C�  C��3C��3C��3C��3C��3C��C��C�  C��3C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C�  C��fC��3C�  C�  C��3C��C��C��3C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C��3C��C��3C��C��3C�  C�  C��3C��3C�  C�  C��C�  C��C�  C��3C��C�  C��fC�  C��3C�  C��C��C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  D   D � D ��D�fDfD� DfD�fD  Dy�D��D� D  D�fD� D  Dy�D  D�fD  D� D  D�fDfD�fD��Dy�D  Dy�DfD�fD��D�fD  D� DfDy�D  D�fD��D� D fD y�D!  D!�fD!��D"� D#fD#� D#��D$�fD%fD%y�D%��D&� D'fD'�fD(fD(y�D(�3D)� D*  D*y�D+  D+�fD,fD,�fD-fD-�fD.  D.y�D.��D/� D0fD0�fD1  D1y�D2  D2y�D2��D3� D4fD4��D5  D5� D6  D6�fD7fD7� D8  D8�fD9fD9� D:  D:� D;  D;y�D<fD<� D=  D=y�D>  D>� D?fD?� D?��D@� DAfDA� DA��DB� DC  DC� DD  DDy�DE  DE� DF  DF� DGfDG� DG��DH� DI  DI� DJ  DJ�fDKfDK� DK��DL� DMfDM� DN  DN� DO  DOy�DO��DPy�DQfDQ�fDR  DRy�DS  DS�fDTfDT�fDUfDUy�DU��DVy�DV��DWy�DW��DXy�DY  DY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^� D_  D_� D`fD`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De�fDf  Df� Dg  Dgy�Dh  Dh� DifDi� Di��Dj� Dk  Dk� Dl  Dl� Dl��Dm� DnfDn��Do  Doy�Dp  Dp� Dp��Dq� DrfDr� DsfDs�fDtfDt��Du  Duy�Du��Dv� Dw  Dw�fDw��Dy��D�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B0p�B8p�B@p�BH�
BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�B�8RB�8RB�8RB�k�B�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�B�8RB�8RB�8RB�8RB�8RC )C)C)C)C5�C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<5�C>)C@5�CB)CD)CF)CH�CJ)CL)CN)CP�CR�CT�CV)CX)CZ)C\�C^)C`)Cb�Cd�Cf�Ch�Cj)Cl)Cn�Cp)Cr)Ct)Cv)Cx�Cz)C|)C~)C�C�C�C�C��C�C�GC�C�GC�GC�C�C��C��C�GC�C��C��C�GC�GC�C��C�C�GC�GC�GC�GC�GC��C��C�C�GC�C�C��C�C�GC�GC�GC�GC�GC�C��C��C�C��zC�GC�C�C�GC��C��C�GC��C�C�GC�GC�GC�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C��C�GC�C�GC��C�GC��C�GC�C�C�GC�GC�C�C��C�C��C�C�GC��C�C��zC�C�GC�C�'�C��C��C�C�GC�C��C�C�GC�GC�C�C�D 
D �
D �D�pDpD�
DpD�pD
D��D �D�
D
D�pD�
D
D��D
D�pD
D�
D
D�pDpD�pD �D��D
D��DpD�pD �D�pD
D�
DpD��D
D�pD �D�
D pD ��D!
D!�pD" �D"�
D#pD#�
D$ �D$�pD%pD%��D& �D&�
D'pD'�pD(pD(��D(�=D)�
D*
D*��D+
D+�pD,pD,�pD-pD-�pD.
D.��D/ �D/�
D0pD0�pD1
D1��D2
D2��D3 �D3�
D4pD4��D5
D5�
D6
D6�pD7pD7�
D8
D8�pD9pD9�
D:
D:�
D;
D;��D<pD<�
D=
D=��D>
D>�
D?pD?�
D@ �D@�
DApDA�
DB �DB�
DC
DC�
DD
DD��DE
DE�
DF
DF�
DGpDG�
DH �DH�
DI
DI�
DJ
DJ�pDKpDK�
DL �DL�
DMpDM�
DN
DN�
DO
DO��DP �DP��DQpDQ�pDR
DR��DS
DS�pDTpDT�pDUpDU��DV �DV��DW �DW��DX �DX��DY
DY�pDZpDZ�pD[pD[�pD\pD\�pD]pD]�pD^pD^�
D_
D_�
D`pD`�
Da �Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De �De�pDf
Df�
Dg
Dg��Dh
Dh�
DipDi�
Dj �Dj�
Dk
Dk�
Dl
Dl�
Dm �Dm�
DnpDn��Do
Do��Dp
Dp�
Dq �Dq�
DrpDr�
DspDs�pDtpDt��Du
Du��Dv �Dv�
Dw
Dw�pDw��Dy� D�D)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A� �A�bA��A���Aʟ�A�v�A�S�A�Aɰ!A�E�A��
A�bNA�+AǬAǡ�AǾwAǛ�A�O�A��TA��`AǼjAǃA�bNA�bNA�ZA��A��mA��/A�ĜA���A� �A���A�^5A�A�A���A��hA� �A��A���A�=qA�oA�O�A�~�A�z�A��A��HA��9A��wA�~�A���A��A�hsA��TA�JA�A���A�C�A��+A�(�A�ZA��A�n�A�n�A�{A���A��A���A�v�A���A�v�A�hsA�ffA�S�A�?}A�5?A�-A� �A�r�A��#A���A��yA��`A��+A�(�A���A���A��A���A�A|�RAx�+Atv�AqXAo"�Al  Ah�Ag�^Afr�AeVAd^5Ab�A_"�A\�uAYS�AT(�AN  AL��AJ�AI�AEAD�HAD(�AC�AChsABr�A@�A?t�A>=qA=l�A<�A:{A6��A4��A3�A3K�A2��A2�DA1A/t�A.�DA,�A*�RA*M�A*VA*bA(ȴA(bA'?}A&v�A$5?A!�A�A��A�hA7LA��AE�A�AA�A7LA5?A�At�A�Av�A|�A��A�A �A`BA
�A	l�A~�A�AȴAhsA"�A�A$�A��A9XA  A;dAC�A �j@��y@�E�@�{@��h@�Ĝ@�r�@�1'@��w@�ƨ@���@�X@�Q�@���@��9@�n�@�G�@�A�@�"�@�-@���@��/@띲@�~�@���@�P@�M�@�=q@��/@���@���@�9X@���@�v�@�K�@ٙ�@�j@�E�@ӥ�@�o@�x�@ϕ�@���@�%@̣�@�E�@���@���@�Q�@��@�K�@ț�@�
=@�?}@�\)@§�@��@��T@��@�n�@��R@�;d@� �@��9@�I�@��m@���@��H@���@�V@�p�@��@���@�o@�ȴ@�M�@�@��T@��^@��@�X@��@��
@�dZ@��@���@���@�n�@�p�@���@���@�z�@�b@�l�@�"�@��y@���@��+@�n�@�M�@�M�@�-@��T@���@�?}@��@�%@���@�(�@���@�C�@�t�@�\)@�33@�+@��H@��T@��h@�@��-@�&�@�I�@�(�@�(�@�(�@��@�  @�|�@��y@�ȴ@���@�ȴ@�=q@���@��^@�X@���@�bN@���@�ƨ@��@��!@���@��+@�ff@���@�G�@��`@���@�1@���@�K�@��@���@�^5@�J@�x�@�G�@�?}@�?}@�/@��@�V@���@���@��@� �@��F@���@�S�@�"�@�@�
=@��@�V@��7@�/@��/@��9@���@��@�j@�A�@�1'@�(�@��@�1@�\)@�v�@�ff@�ff@�V@�5?@���@���@���@��@�/@���@��D@�A�@��w@�dZ@���@���@��R@��R@��!@���@��\@�n�@�M�@�$�@���@��h@�G�@�/@�V@���@���@��@�Q�@�9X@�1@��;@���@�"�@��@��R@��+@�v�@�ff@�-@���@��T@��-@�hs@�?}@��@��/@��j@��D@�r�@�A�@�b@��;@���@�ƨ@�|�@�o@��H@���@���@�^5@��#@��-@��h@��@�bN@�(�@� �@��@�t�@�+@���@��@�v�@��T@��h@�O�@�&�@��@��9@�Z@� �@�;@}B�@f1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A� �A�bA��A���Aʟ�A�v�A�S�A�Aɰ!A�E�A��
A�bNA�+AǬAǡ�AǾwAǛ�A�O�A��TA��`AǼjAǃA�bNA�bNA�ZA��A��mA��/A�ĜA���A� �A���A�^5A�A�A���A��hA� �A��A���A�=qA�oA�O�A�~�A�z�A��A��HA��9A��wA�~�A���A��A�hsA��TA�JA�A���A�C�A��+A�(�A�ZA��A�n�A�n�A�{A���A��A���A�v�A���A�v�A�hsA�ffA�S�A�?}A�5?A�-A� �A�r�A��#A���A��yA��`A��+A�(�A���A���A��A���A�A|�RAx�+Atv�AqXAo"�Al  Ah�Ag�^Afr�AeVAd^5Ab�A_"�A\�uAYS�AT(�AN  AL��AJ�AI�AEAD�HAD(�AC�AChsABr�A@�A?t�A>=qA=l�A<�A:{A6��A4��A3�A3K�A2��A2�DA1A/t�A.�DA,�A*�RA*M�A*VA*bA(ȴA(bA'?}A&v�A$5?A!�A�A��A�hA7LA��AE�A�AA�A7LA5?A�At�A�Av�A|�A��A�A �A`BA
�A	l�A~�A�AȴAhsA"�A�A$�A��A9XA  A;dAC�A �j@��y@�E�@�{@��h@�Ĝ@�r�@�1'@��w@�ƨ@���@�X@�Q�@���@��9@�n�@�G�@�A�@�"�@�-@���@��/@띲@�~�@���@�P@�M�@�=q@��/@���@���@�9X@���@�v�@�K�@ٙ�@�j@�E�@ӥ�@�o@�x�@ϕ�@���@�%@̣�@�E�@���@���@�Q�@��@�K�@ț�@�
=@�?}@�\)@§�@��@��T@��@�n�@��R@�;d@� �@��9@�I�@��m@���@��H@���@�V@�p�@��@���@�o@�ȴ@�M�@�@��T@��^@��@�X@��@��
@�dZ@��@���@���@�n�@�p�@���@���@�z�@�b@�l�@�"�@��y@���@��+@�n�@�M�@�M�@�-@��T@���@�?}@��@�%@���@�(�@���@�C�@�t�@�\)@�33@�+@��H@��T@��h@�@��-@�&�@�I�@�(�@�(�@�(�@��@�  @�|�@��y@�ȴ@���@�ȴ@�=q@���@��^@�X@���@�bN@���@�ƨ@��@��!@���@��+@�ff@���@�G�@��`@���@�1@���@�K�@��@���@�^5@�J@�x�@�G�@�?}@�?}@�/@��@�V@���@���@��@� �@��F@���@�S�@�"�@�@�
=@��@�V@��7@�/@��/@��9@���@��@�j@�A�@�1'@�(�@��@�1@�\)@�v�@�ff@�ff@�V@�5?@���@���@���@��@�/@���@��D@�A�@��w@�dZ@���@���@��R@��R@��!@���@��\@�n�@�M�@�$�@���@��h@�G�@�/@�V@���@���@��@�Q�@�9X@�1@��;@���@�"�@��@��R@��+@�v�@�ff@�-@���@��T@��-@�hs@�?}@��@��/@��j@��D@�r�@�A�@�b@��;@���@�ƨ@�|�@�o@��H@���@���@�^5@��#@��-@��h@��@�bN@�(�@� �@��@�t�@�+@���@��@�v�@��T@��h@�O�@�&�@��@��9@�Z@� �@�;@}B�@f1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�7B
�7B
�=B
�DB
�JB
��B
��B
�B
�B$�BXB`BBW
BVBK�BR�B]/B]/B^5B�B��B��B��B��B��B��B��B��B�B�9BĜBǮB�)B  B1'BM�BL�BJ�BN�BW
BYBQ�BW
BP�BT�B]/B\)BXB^5BcTBl�BffBT�BE�B6FBC�B8RB9XB1'B �B��B�NB��BÖB��Bz�Br�Bt�Bp�BVB6FB2-B0!B/B-B)�B(�B(�B%�B�B
�`B
��B
ŢB
�RB
��B
�hB
}�B
l�B
YB
6FB
�B
1B	�B	��B	ǮB	��B	�?B	��B	��B	�JB	~�B	x�B	l�B	T�B	F�B	2-B	oB��B�B�sB�HB��B��B��BɺBǮBÖB�}B�dB�RB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�+B�B� B� B�B� B}�B|�B|�B{�By�Bv�Bt�Br�Bq�Bp�Bk�Bl�Bk�BjBjBk�Bn�Bn�Bn�By�B|�B|�B|�B�+B�DB�PB�\B�{B��B�bB�hB�oB�{B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�RB�LB�9B�FB�qB�qB�qB�wB��BÖBĜBŢBȴB��B�
B�B�B�
B�B�B�B��B�/B�ZB�B�B��B��B��B��B��B��B	B	JB	49B	7LB	9XB	=qB	>wB	?}B	@�B	A�B	B�B	C�B	L�B	M�B	P�B	P�B	P�B	Q�B	W
B	[#B	[#B	\)B	^5B	aHB	cTB	dZB	e`B	hsB	iyB	l�B	n�B	o�B	r�B	s�B	t�B	t�B	t�B	t�B	u�B	{�B	z�B	~�B	�B	�B	�B	�B	�%B	�DB	�PB	�VB	�\B	�hB	�hB	�bB	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�?B	�LB	�XB	�^B	�dB	�wB	�}B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�NB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
PB
\B
\B
\B
\B
bB
bB
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
#B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�7B
�7B
�=B
�DB
�JB
��B
��B
�B
�B$�BXB`BBW
BVBK�BR�B]/B]/B^5B�B��B��B��B��B��B��B��B��B�B�9BĜBǮB�)B  B1'BM�BL�BJ�BN�BW
BYBQ�BW
BP�BT�B]/B\)BXB^5BcTBl�BffBT�BE�B6FBC�B8RB9XB1'B �B��B�NB��BÖB��Bz�Br�Bt�Bp�BVB6FB2-B0!B/B-B)�B(�B(�B%�B�B
�`B
��B
ŢB
�RB
��B
�hB
}�B
l�B
YB
6FB
�B
1B	�B	��B	ǮB	��B	�?B	��B	��B	�JB	~�B	x�B	l�B	T�B	F�B	2-B	oB��B�B�sB�HB��B��B��BɺBǮBÖB�}B�dB�RB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�+B�B� B� B�B� B}�B|�B|�B{�By�Bv�Bt�Br�Bq�Bp�Bk�Bl�Bk�BjBjBk�Bn�Bn�Bn�By�B|�B|�B|�B�+B�DB�PB�\B�{B��B�bB�hB�oB�{B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�RB�LB�9B�FB�qB�qB�qB�wB��BÖBĜBŢBȴB��B�
B�B�B�
B�B�B�B��B�/B�ZB�B�B��B��B��B��B��B��B	B	JB	49B	7LB	9XB	=qB	>wB	?}B	@�B	A�B	B�B	C�B	L�B	M�B	P�B	P�B	P�B	Q�B	W
B	[#B	[#B	\)B	^5B	aHB	cTB	dZB	e`B	hsB	iyB	l�B	n�B	o�B	r�B	s�B	t�B	t�B	t�B	t�B	u�B	{�B	z�B	~�B	�B	�B	�B	�B	�%B	�DB	�PB	�VB	�\B	�hB	�hB	�bB	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�?B	�LB	�XB	�^B	�dB	�wB	�}B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�NB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
PB
\B
\B
\B
\B
bB
bB
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
#B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191649                              AO  ARCAADJP                                                                    20181005191649    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191649  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191649  QCF$                G�O�G�O�G�O�8000            