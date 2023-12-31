CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:29Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125829  20190408133247  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @���=p��1   @����}8&@4�n��O��bۥ�S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��fD�C3D��3D��3D���D�,�D���D�ٚD�	�D�33D�� D��3D��3D�33Dډ�D��fD�fD�9�D�s3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A (�A!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DA
DA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL
DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DV
DV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
Db
Db�
Dc
Dc�
Dd
Dd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk
Dk�
Dl
Dl�
Dm
Dm�pDn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dt�
Dt�pDy�
D���D�F�D���D�ƸD��D�0RD��D��D�D�6�D���D�ƸD���D�6�DڍD���D�	�D�=D�v�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�JA�(�A�&�A�$�A�(�A�(�A�&�A�&�A�&�A�&�A�/A�1'A�1'A�1'A�5?A�5?A�7LA�9XA�9XA�9XA�7LA�5?A�+A�$�Aβ-A�Q�A��A�Q�A�-A�1A���A�A�r�A��#AƬA��
A��A��/A©�A�jA�bA���A��PA�^5A�ffA�x�A��A���A�1A�?}A�|�A��uA��A�1A��A�bA���A���A��A��HA���A��uA�jA���A�r�A�I�A�%A��wA��TA�oA��FA�5?A��A�dZA��A�z�A��A��\A�S�A��7A�ĜA���A�+A�
=A�ĜA�^5A�ZA���A�?}A���A�E�A�A�bNA�z�A���A��uA���A�1A���A��HA���A�x�A�1'A��wA��A�v�A�  A��!A�E�A�ȴA�ĜA�%A�n�A�1A�=qA�33AdZA|M�AyƨAxn�Aw�hAu�Aq33Amt�AkO�Ai��Af��Ac�wAa�A_��A^~�A[�wAZQ�AW�
AVn�AShsAM?}AK�AH1'AG+AF�9AD-A@�+A?S�A>�RA=&�A;O�A9p�A8�/A7l�A4�A3\)A1��A.ĜA-�TA-��A-l�A,�uA+��A)��A'�^A&VA%�^A$bA"�`A" �A!S�AG�AffAK�A �A�#A�A�A �A�TAn�A?}AbNA��AM�A��A"�AK�A\)A+Ar�AG�AdZA�Ar�A1A��A��A��A�hA
$�A	VA��A�PA^5A�A�A�mA�@��P@��!@�p�@�&�@��@�P@�~�@�r�@�K�@���@���@��@�^@�X@畁@���@��T@㝲@��@ߥ�@��@�+@� �@ְ!@���@ԣ�@��
@ӶF@�S�@ӍP@�(�@Դ9@��#@�|�@�@���@ָR@�J@ՙ�@Ձ@�O�@� �@ӝ�@��H@�M�@Ѻ^@��@�t�@�o@�@���@ˍP@�dZ@ɺ^@�(�@Ɨ�@��@��m@�o@�=q@�-@���@���@�5?@�`B@���@�@�~�@�"�@��@��T@���@��R@��@��T@�hs@���@��
@��m@��F@��@��@���@�;d@���@��P@���@��y@�J@�v�@�33@�5?@�r�@�;d@��T@�?}@�j@�I�@��@�b@��H@�n�@�@�n�@��H@��R@�J@�V@��@���@��+@���@�x�@��@��9@�r�@�  @�S�@�o@���@���@�/@�/@���@�I�@�b@���@��;@��@�t�@��@���@���@���@�M�@�$�@�5?@�@���@��h@���@�7L@��@��@�  @��@�z�@�1@�"�@�J@���@���@�hs@�`B@�&�@��j@�1'@�Z@��/@���@��j@�z�@�1'@� �@��@�t�@�
=@�@���@�=q@���@��@�Ĝ@���@�r�@�I�@�9X@�  @�;d@��y@��H@��H@���@��+@��^@��@��@�%@��D@��@��w@�\)@��H@��\@��\@�V@�{@���@��T@��7@���@��@��R@���@��@�o@�n�@�x�@��u@��/@�&�@��j@�j@�Q�@�9X@��@�t�@��y@�v�@�E�@�5?@�-@��h@�Z@�l�@�33@���@�$�@��#@���@�O�@��9@�j@��m@��P@�K�@���@�ȴ@���@���@�~�@�^5@�$�@���@��@�I�@�(�@��D@�9X@�b@�z�@��j@�O�@���@�V@��^@�O�@��@���@���@�z�@�1'@�A�@��D@���@��u@�r�@�Z@���@�K�@��F@���@�dZ@�K�@�@���@��+@���@��@�@���@��!@�7L@�G�@xbN@n$�@f��@^��@V�R@O��@GK�@@��@9��@4�D@/l�@*J@$��@!�@�@�`@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A�JA�(�A�&�A�$�A�(�A�(�A�&�A�&�A�&�A�&�A�/A�1'A�1'A�1'A�5?A�5?A�7LA�9XA�9XA�9XA�7LA�5?A�+A�$�Aβ-A�Q�A��A�Q�A�-A�1A���A�A�r�A��#AƬA��
A��A��/A©�A�jA�bA���A��PA�^5A�ffA�x�A��A���A�1A�?}A�|�A��uA��A�1A��A�bA���A���A��A��HA���A��uA�jA���A�r�A�I�A�%A��wA��TA�oA��FA�5?A��A�dZA��A�z�A��A��\A�S�A��7A�ĜA���A�+A�
=A�ĜA�^5A�ZA���A�?}A���A�E�A�A�bNA�z�A���A��uA���A�1A���A��HA���A�x�A�1'A��wA��A�v�A�  A��!A�E�A�ȴA�ĜA�%A�n�A�1A�=qA�33AdZA|M�AyƨAxn�Aw�hAu�Aq33Amt�AkO�Ai��Af��Ac�wAa�A_��A^~�A[�wAZQ�AW�
AVn�AShsAM?}AK�AH1'AG+AF�9AD-A@�+A?S�A>�RA=&�A;O�A9p�A8�/A7l�A4�A3\)A1��A.ĜA-�TA-��A-l�A,�uA+��A)��A'�^A&VA%�^A$bA"�`A" �A!S�AG�AffAK�A �A�#A�A�A �A�TAn�A?}AbNA��AM�A��A"�AK�A\)A+Ar�AG�AdZA�Ar�A1A��A��A��A�hA
$�A	VA��A�PA^5A�A�A�mA�@��P@��!@�p�@�&�@��@�P@�~�@�r�@�K�@���@���@��@�^@�X@畁@���@��T@㝲@��@ߥ�@��@�+@� �@ְ!@���@ԣ�@��
@ӶF@�S�@ӍP@�(�@Դ9@��#@�|�@�@���@ָR@�J@ՙ�@Ձ@�O�@� �@ӝ�@��H@�M�@Ѻ^@��@�t�@�o@�@���@ˍP@�dZ@ɺ^@�(�@Ɨ�@��@��m@�o@�=q@�-@���@���@�5?@�`B@���@�@�~�@�"�@��@��T@���@��R@��@��T@�hs@���@��
@��m@��F@��@��@���@�;d@���@��P@���@��y@�J@�v�@�33@�5?@�r�@�;d@��T@�?}@�j@�I�@��@�b@��H@�n�@�@�n�@��H@��R@�J@�V@��@���@��+@���@�x�@��@��9@�r�@�  @�S�@�o@���@���@�/@�/@���@�I�@�b@���@��;@��@�t�@��@���@���@���@�M�@�$�@�5?@�@���@��h@���@�7L@��@��@�  @��@�z�@�1@�"�@�J@���@���@�hs@�`B@�&�@��j@�1'@�Z@��/@���@��j@�z�@�1'@� �@��@�t�@�
=@�@���@�=q@���@��@�Ĝ@���@�r�@�I�@�9X@�  @�;d@��y@��H@��H@���@��+@��^@��@��@�%@��D@��@��w@�\)@��H@��\@��\@�V@�{@���@��T@��7@���@��@��R@���@��@�o@�n�@�x�@��u@��/@�&�@��j@�j@�Q�@�9X@��@�t�@��y@�v�@�E�@�5?@�-@��h@�Z@�l�@�33@���@�$�@��#@���@�O�@��9@�j@��m@��P@�K�@���@�ȴ@���@���@�~�@�^5@�$�@���@��@�I�@�(�@��D@�9X@�b@�z�@��j@�O�@���@�V@��^@�O�@��@���@���@�z�@�1'@�A�@��D@���@��u@�r�@�Z@���@�K�@��F@���@�dZ@�K�@�@���@��+@���@��@�@���G�O�@�7L@�G�@xbN@n$�@f��@^��@V�R@O��@GK�@@��@9��@4�D@/l�@*J@$��@!�@�@�`@��@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBP�BP�BR�BYBXBYBYBXBW
BXBXBXBYBYBYBYBYBYBYBYBYBYBYBYBYBW
BS�BQ�BQ�BO�BP�BQ�BP�BJ�BS�BVBXB]/BiyB{�B�B��B��B�B�B�'BB�B)�B1'B<jBF�BQ�BS�B�uB��B�3B��B��B�{B�hB�DB�JB�+B�+B�\B��B�uB�oB�JB�+B�%B��B�uB�JB�1B�B~�Bu�BffBQ�BH�B?}B=qB5?B#�B
=B�B�/BƨB�RB�B��B�bB�1By�BbNB9XB,B'�B5?B9XB<jB0!B�B{B+B
�B
�B
�TB
�)B
��B
ÖB
��B
��B
�\B
gmB
A�B
VB	�B	�BB	��B	ɺB	�3B	�B	]/B	D�B	/B	VB��B��B�fB�5B�B��B�dB�!B��Bw�Bk�Bn�Br�Br�Bq�Bn�BjBgmBjBl�By�B{�B}�B|�Bv�Bq�BiyBhsBhsBffBe`BcTB`BB^5B]/B\)B]/B[#BYBVBR�BP�BS�BcTBgmBjBt�B�{B��B��B�uB��B��B��B��B��B��B�B�3B�^B�B�fB�fB�NB�5B�B��B��B��B�B}�B�Bm�Bm�Bn�Bm�BjBiyBdZBbNB^5BW
BQ�BJ�BD�BC�BC�BC�BE�BH�BM�BQ�BQ�B]/B`BB\)BYBZBW
BZBZB]/B^5BaHBcTBgmBq�B� B�=B��B��B�RB�jB�qB�wBŢBɺB��B�
B��B��B��B��B�B�
B�)B�5B�5B�)B�#B�#B�B�
B��B��B��B��B��B�
B�
B�B�;B�;B�HB�B��B��B	B��B��B��B��B��B	  B	B	B	B	B	B	B	B	DB	\B	bB	{B	{B	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	)�B	0!B	0!B	33B	7LB	=qB	@�B	B�B	D�B	J�B	Q�B	VB	W
B	XB	[#B	^5B	_;B	aHB	aHB	`BB	cTB	cTB	bNB	bNB	e`B	e`B	dZB	e`B	hsB	iyB	k�B	m�B	p�B	s�B	u�B	w�B	y�B	{�B	}�B	}�B	�B	�B	�%B	�1B	�1B	�1B	�JB	�hB	�{B	��B	�{B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�3B	�LB	�XB	�XB	�XB	�^B	�dB	�jB	�dB	�dB	�dB	�^B	�dB	�^B	�dB	�qB	�}B	ÖB	ŢB	ǮB	ǮB	��B	��B	ȴB	ƨB	ŢB	ǮB	ɺB	��B	ȴB	ƨB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�/B	�NB	�mB	�B	�yB	�sB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
+B
hB
�B
#�B
+B
1'B
7LB
>wB
A�B
G�B
M�B
R�B
XB
]/B
aHB
e`B
hsB
l�B
p�B
t�111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BP�BP�BR�BYBXBYBYBXBWBX	BXBXBYBYBYBYBYBYBYBYBYBYBYBYBYBWBS�BQ�BQ�BO�BP�BQ�BP�G�O�BS�BU�BXB]'BioB{�B�B��B��B�B�B�!B �B~B)�B1B<cBF�BQ�BS�B�kB��B�'B��B��B�qB�_B�8B�BB�"B�"B�RB�xB�jB�gB�@B�%B�B�}B�oB�AB�*B�B~�Bu�Bf\BQ�BH�B?qB=hB55B#�B
4B�B�%BƟB�IB�B��B�XB�%By�BbBB9NB+�B'�B55B9LB<aB0B�BqBB
�B
�rB
�HB
�B
��B
ÍB
��B
��B
�SB
g`B
A}B
JB	�B	�7B	��B	ɮB	�)B	�B	]$B	D�B	/B	JB��B��B�[B�(B��B��B�WB�B��Bw�BkzBn�Br�Br�Bq�Bn�BjsBgbBjqBl�By�B{�B}�B|�Bv�Bq�BikBhgBhfBfYBeTBcJB`6B^+B]#B\B]"B[BYBU�BR�BP�BS�BcEBg_BjrBt�B�mB��B��B�hB�yB�vB��B��B��B��B�B�$B�RB��B�YB�VB�BB�'B�B��B�{B��B�B}�B�Bm�Bm�Bn�Bm�BjsBimBdMBb?B^(BV�BQ�BJ�BD�BC�BC�BC�BE�BH�BM�BQ�BQ�B]!B`3B\BY
BZBV�BZBZB] B^&Ba;BcEBg`Bq�B�B�/B��B��B�DB�]B�dB�gBœBɭB��B��B��B��B��B��B��B��B�B�'B�(B�B�B�B�B��B��B��B˸BʲB��B��B��B�	B�*B�-B�;B�vB��B��B	 �B��B��B��B��B��B��B	 �B	�B	B		B	B	B	B	5B	LB	TB	kB	oB	eB	}B	�B	�B	�B	�B	qB	�B	�B	 �B	)�B	0B	0B	3$B	7?B	=dB	@tB	B�B	D�B	J�B	Q�B	U�B	V�B	XB	[B	^%B	_-B	a;B	a;B	`3B	cFB	cEB	bAB	bAB	eSB	eQB	dKB	eRB	hdB	ihB	kwB	m�B	p�B	s�B	u�B	w�B	y�B	{�B	}�B	}�B	��B	�B	�B	�"B	�$B	�"B	�<B	�YB	�lB	�tB	�nB	�XB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�&B	�%B	�=B	�IB	�IB	�IB	�PB	�UB	�]B	�VB	�VB	�VB	�OB	�XB	�QB	�TB	�eB	�nB	ÈB	ŒB	ǠB	ǢB	ʴB	ʲB	ȥB	ƛB	ŕB	ǠB	ɫB	ʳB	ȧB	ƜB	ŕB	ɫB	̾B	̽B	̾B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	̽B	ɫB	ɫB	ǠB	ǠB	ȦB	̿B	̿B	˸B	˸B	̿B	̼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�@B	�^B	�oB	�lB	�eB	�`B	�eB	�gB	�dB	�pB	�uB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
B
XB
�B
#�B
*�B
1B
7?B
>hB
AyB
G�B
M�B
R�B
XB
]B
a<B
eTB
hdB
l~B
p�B
t�111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332472019040813324720190408133247  AO  ARCAADJP                                                                    20181121125829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125829  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133247  IP                  G�O�G�O�G�O�                