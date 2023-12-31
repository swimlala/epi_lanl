CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-07-22T09:45:44Z creation;2019-07-22T09:45:46Z conversion to V3.1;2022-08-02T05:12:20Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190722094544  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_017                    2C  D   APEX                            8420                            2.11.2                          846 @������1   @���0쨀@,`qu��d.1&�y1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@y��@���A   A   A@  A`  A�  A�  A���A���A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�ffB�  B�  B���C   C  C  C  C�C
�C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^33C`  Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@�Q�@���Ap�A!AAAap�A��RA���A�p�A�33A��HAиRA�ffA�RB G�B\)BffB\)B ffB(Q�B0ffB8p�B@\)BHp�BPz�BX\)B`ffBhz�Bpp�BxffB�(�B��B�B�B��qB��RB�{B�(�B�(�B�.B�.B��B�#�B�8RB�Q�B�{B�  B�#�B�(�B�#�B�B�B�(�B�u�B�k�B��B�(�B�8RB�L�B��B��\B�33B��B�\C {C)C�C#�C33C
0�C{C{C
=C�C�C
C\C\C�C
C 
C"{C$)C&)C(
C*0�C,.C.)C0�C2
=C4\C6{C8{C:
C<
C>�C@
CB�CD{CF{CH�CJ)CL)CN
CP{CR)CT)CV
CX\CZ�C\(�C^G�C`{Cb�Cd�Cf�Ch�Cj
Cl)Cn�Cp
Cr)Ct�Cv
Cx�Cz)C|
C~�C�\C��C��C�
=C��C�
=C�C��C��C��C��C��C��C��C�C�C��C�fC��C�C�C�C��C�{C��C�
=C��C��C��C�C�\C�\C�C��C��C��C��C�C�
=C��C�
=C�C�\C�\C��C��C�C�C��C�
=C��C�\C��C�C�\C��C��C��C��C��C��C��C��C��C��C��C�
=C��C�C�\C��C��C��C��C�C��C�
=C��C��C��C��C�
=C�
=C��C�C��C�
=C��C�C�C�
=C��C�
=C�C�C��C��C��C�C��C�C�\C�C�C��C��C��C�3C��C��C��C��C�{C��C��C�C��C��C��C�C�C�
=C��C�C��C�
=C��C��D RD ��D�D�fD�D�fD�D��D
D�fDfD�{DD��DfD�fDfD�fD	
D	��D
RD
�fD�D�
DRD�RD
D�{D�D��DfD�{D�D��D�D�D
D�RD
D�
DRD�
DfD��DfD��D3D�{DfD��DD�DD�{DD�fD�D��DfD�fDRD�RD
D�
D �D �
D!RD!�RD"�D"�fD#fD#�D$fD$�fD%fD%�
D&�D&��D'
D'�
D(�D(��D)�D)��D*fD*��D+fD+�
D,fD,�fD-	�D-��D.�D.�
D/D/��D0{D0��D1fD1�fD2�D2�D3D3�fD4�D4�RD5�D5�D6�D6�fD7�D7��D8fD8�D9D9�fD:fD:�D;3D;�{D<D<�D=
D=��D>fD>�fD?�D?�
D@{D@�{DA�DA�fDBDB�DC�DC��DD
DD��DE�DE�RDFRDF�RDGfDG�{DHfDH��DIDI�DJ�DJ�fDK�DK��DL
DL�RDM	�DM��DNfDN��DO�DO�fDPRDP��DQ�DQ��DRfDR�
DS
DS�fDTfDT�
DU
DU��DV�DV��DWfDW��DXRDX�DYDY�DZ{DZ�D[{D[�fD\�D\��D]�D]��D^�D^�D_D_��D`D`�Da�Da��DbfDb��DcfDc�fDd
Dd�
DefDe��Df
Df��DgRDg��DhDh�fDi�Di�RDj�Dj�fDk{Dk�DlDl��Dm�Dm�DnDn�Do{Do�Dp
Dp�RDqRDq�fDrfDr��DsRDs�
DtRDt�
Du{Du��DvfDv�{Dw
Dw��DxfDx��Dy{Dy�fDzfDz�{D{�D{�RD|
D|�fD}�D}��D~�D~�
D	�D��D��D�B=D���D��3D��D�C�D���D���D��D�C3D��3D���D��D�B�D��3D���D��D�B�D���D��3D�3D�C�D��3D���D�{D�D{D��)D���D�=D�B=D���D�D��D�C3D���D�D��D�B�D��3D�D��D�D)D��)D�ÅD�3D�C3D���D�D��D�B�D���D��3D��D�B�D��3D�ÅD�)D�D)D���D��)D�)D�D{D���D�ÅD��D�C�D��)D��)D��D�C�D���D�ÅD�3D�C�D���D�D��D�D)D���D�ÅD�3D�C3D��{D���D��D�C�D���D��{D��D�B�D��3D���D��D�D{D��{D���D��D�B=D���D��{D��D�D)D��{D���D��D�C�D���D��)D��D�B=D���D�D��D�C3D��3D�ÅD�3D�C�D���D���D�{D�D)D���D���D��D�C�D��)D���D��D�B�D���D��3D��D�B=D���D��3D��D�B�D���D�D��D�B�D��{D�ÅD��D�B�D���D�D�HD�B=D��3D�ÅD��D�D{D���D���D��D�D{D���D���D��D�D)D��3D�D��D�B�D���D��3D�=D�B=D��=D���D��D�B=D���D�D�=D�B=D���D���D��D�C�D���D��3D��D�B�D��)D�ÅD�3D�D)D���D�D�3D�C�D���D�ÅD�)D�C3D���D�D��D�C�D���D���D��D�B�D���D���D�)D�C�D���D���D��D�C3D��)D��3D�=D�B�D���D���D��D�B�D���D���D��D�C3D���D��3D��D�C3D���D��3D��D�B=D��3D��)D��D�C�D���D��=D��D�C3D���D�D��D�C3D���D�D�3D�C�D���D��3D��D�B�D���D���D�)D�B�D��=D�ÅD��D�C3D���D���D�3D�A�D�D�ÅD�3D�C3DÃ�D��3D��D�C3Dă�D���D�{D�C�Dł=D���D��D�C3DƂ�D���D��D�D)Dǃ�D�ÅD�3D�B=Dȃ3D��)D��D�C�DɃ�D���D��D�D)Dʃ�D��3D��D�C�D˂�D�D��D�B�D̃�D��3D��D�C�D̓3D���D��D�C3D΃3D��=D�=D�B=Dς=D��3D�3D�C�DЃ�D���D�3D�B�Dт�D��=D��D�C�D҃�D�ÅD�3D�B�DӃ3D��3D��D�C�DԂ�D��3D�{D�D{DՃ�D��3D�3D�C�Dք{D���D��D�C�Dׄ)D�ÅD��D�C3D؂=D���D�3D�C3Dك3D�ÅD��D�C�Dڃ�D���D��D�C�Dۄ)D��{D��D�B�D܃3D���D��D�C�D݂�D�ÅD�3D�B�Dރ�D��{D��D�B=D߂�D��3D��D�C3D��)D���D��D�C3DჅD��3D��D�C�D��D���D��D�C�D�)D�ÅD��D�B�D�3D���D��D�A�D傏D�ÅD��D�C3D��D���D��D�C�D炏D��3D�)D�C3D��D��3D��D�C3D�{D��3D��D�B�D��D��)D��D�C3D��D�D��D�C�D�{D��{D��D�C3D�)D�ÅD�)D�C�D�=D�D��D�A�D�=D��3D�=D�B=D���D��)D��D�B�D�=D��=D��D�B�D��D���D��D�B�D��D���D�3D�C�D��D�ÅD�{D�D{D���D���D�=D�C�D���D��=D��D�C�D��3D���D��D�C�D��{D���D��D�C�D��3D�D�{D�ED��)D�ÅD�3D�C3D���D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�3�A�9�A�?�A�HA�A�A�@�A�A�A�C�A�C�A�B�A�?HA�B�A�D�A�7A�IA���A��QA��A���A��A���A�  A���A��rA��A��A�uA���A��A��yA��/A��jA� �A�ѷA�HA�{A�	7A�HAў�A�G�A�\�A�`BA�*�A�� A�N�A�"�AƸRAçRA��)A�Z�A�MA�	A���A��A���A��jA��'A�3�A�;�A�X�A���A���A�<6A�[�A�`vA���A���A�:�A���A�V�A�n�A��hA���A�;0A�m]A���A���A���A��fA���A���A�{A�4�A���A|�$Az4At+Aq��Ao`�Ak_Ag�WAd_A`9�A^%FA\��AY��AX�AUtTAR��AO[WAL�AI�TAI�AH,�AF��ADa|AB_A?�A=VmA<+A9�9A6i�A3��A1��A0&�A/�A.��A-{�A,��A-"hA-��A,�A+�A*�BA*1�A)�A)�[A)!-A(x�A(�A'Z�A&�HA& iA%�A%��A%~�A$�&A$&�A"H�A ȴA �+A OA��A=A�A�:A(�A=�A��A��A��A�rAB[AO�A��A\�A��AݘA��A��A�AB�A�Al�A~A�A�A�A�[AL�AOA�Am�A�DAT�A��AU�A?A�A��Ag8A��A��AffAOvA�mA��A�WA��A�A��ATaA�A1�A7A��A:*A9XA�AoiA��A��A
�A
�rA
?A
�A	�A	��A	N<A	A��AMA��A7�AaA˒A<�A��AC-AϫA�AtTA+A �mA ��A zA !-@��*@���@�S�@��b@��d@�m]@�N<@��	@��O@�/�@�ԕ@���@��@���@��6@���@�J#@�b�@�;@�<�@�%@��*@�a@�  @�i�@�H�@���@�K�@�@�C@���@�H@���@�O�@���@��r@�Z@�C�@�Q�@�7@�j@�@@�kQ@�c�@�s�@�<6@���@�{J@��f@�W�@��@�=@��@��@�\�@�8�@�W�@�r@⤩@�\@��@��@��@�{@߲�@��@�\�@�;@�m]@߯�@ߗ�@�x�@�RT@��@��8@���@���@���@�j@���@�Y�@���@܎�@�c @�
�@�zx@�@�}V@�!@� \@�-�@�u%@��P@�u%@�D�@�%�@��T@ӝ�@�e�@�[W@�X�@���@�z�@�6@�2�@�G@��@Ѽ@��@��@ϣn@�V@�\�@�%�@�@�hs@��@̀�@�@�?}@��`@ʴ9@�|�@�}�@ȫ6@�Ov@ǲ-@�C�@�)_@ƯO@ŷ@���@��@�H@�@���@���@�5?@�;�@��j@�2a@��@���@��+@��?@���@�ϫ@���@�v`@�T�@��/@��I@�ff@�@�@�@��W@�tT@�iD@�(@�Ɇ@�H�@�dZ@��@��D@��@���@�X�@�@@��j@�h�@�1'@�{@��Z@��@��@�P�@��f@���@��@�9X@�	�@���@��)@���@�U�@��@���@�C-@��K@���@��	@���@�~�@�[�@��@�P�@��|@�H�@��N@���@�Y�@�&�@��s@�R�@�˒@���@�S&@�&@��@�ی@���@��+@�4n@�� @���@��@�e,@�(�@��@���@��@�W�@���@���@�W?@��@�q�@���@���@�iD@��@���@��o@��9@�y�@�+�@���@�r�@�Ta@��>@��t@�n/@�>�@�@@���@�YK@�@��^@�`B@�4�@��f@���@��D@��@���@���@�9�@��|@���@�c�@�?@�'R@�	@�G@�@��@�Q�@���@��@��A@���@�"�@��@��@��b@�h
@��@���@��@�ԕ@��@�F@��X@���@�H�@�=q@�!@�J@���@��@���@�2a@��@���@��m@���@�e@���@�o @��"@��b@�Z@�?�@�6�@�
�@��@�A�@��$@�<�@��.@��>@��K@��"@��@��e@�m�@�'R@���@�|@�J�@�7L@�6z@�)_@��@��y@���@��\@�Ft@��D@��6@��H@���@���@�w2@�P�@��@�ߤ@�n�@��o@���@�J�@�;@��9@��@�m�@�GE@�:�@�&�@�x@��@���@��d@�x@�ȴ@�n�@�0U@���@���@�Mj@���@�Ɇ@���@�{�@�g8@�E�@�@�|�@��@�ی@���@��o@�;�@�"h@�r@�a@(@}�9@}f�@|��@|S�@{��@z�M@z�@z@�@y��@y/@y@@x�?@w��@wE9@w�@v��@vl�@v8�@vu@ua�@t<�@sC�@r�s@r�x@r^5@r	@qhs@pی@pw�@pA�@oH�@n��@n�X@n6�@mzx@mrG@mf�@m2a@l�o@k�@k��@k�P@ka@j�8@j҉@j��@j0U@i�H@i:�@h�j@hM@g�r@g�g@g�@g�:@gY@f��@f3�@e�z@d��@d:�@c�@c��@co�@b��@bE�@b!�@be@a��@a^�@`��@`-�@_�:@_6z@^��@^E�@]��@]%@\oi@\M@[�0@[;d@Z�"@Z��@Z��@ZB[@Y�@Y|@Y/@X�U@X`�@X(�@Xb@W�@W8@V��@VH�@U��@Ux�@T�|@Te�@T2�@S�A@S��@SdZ@SX�@S@R�A@Q�o@Q�@Q<6@QV@P�@P>B@P'R@P�@O�A@O�a@OZ�@N��@Na|@N#:@Mϫ@M|@ML�@M@L��@LQ�@K��@K�	@KC�@J�\@JGE@I�@Ij@H�j@H��@H~(@HQ�@H@H�@G��@G6z@Fc @F&�@E�T@Eo @D�e@DI�@D2�@D�@C��@C�@B��@Bc @A�T@A\�@@�@@j@?�@?qv@?P�@?@>kQ@>	@=ϫ@=�@=A @=(�@<�5@<Xy@;�a@;~�@:�@:��@:�A@:�@9�S@90�@8��@8�@8@7��@7��@7�V@7\)@7"�@7�@7�@6�c@6�B@6��@5�Z@5w2@5<6@4�`@4��@4�D@4j@4D�@4M@3��@3��@3�	@3X�@3�@2i�@2.�@24@2_@1��@1�@1�@1��@1�t@1c@0�5@07�@/�;@/@O@.��@.Ta@.J@-�@-�C@-8�@,�P@,�E@,j@,:�@+�@+��@+O@+�@*�@*��@*H�@)�T@)�-@)�S@)s�@)\�@)B�@)@(�$@(�o@(>B@(b@'��@'��@'v`@&�"@&\�@&3�@&8�@&J@%�@%��@%2a@$�@$��@$z�@$N�@$�@#�k@#�@#x@#o�@#E9@"�2@"��@"��@"s�@"W�@"#:@" �@!�)@!�@!�j@!�3@!�=@!4@!;@ �@ ی@ �9@ ��@ w�@ @� @�6@g�@9�@�@�M@�x@kQ@E�@?@3�@u@��@�@�@Vm@��@�[@Xy@�@��@~�@.I@��@�<@��@#:@��@�>@��@��@�)@�@s�@�@��@�e@��@z�@`�@@�+@�W@�;@��@4�@o@�@�@��@�2@��@�!@��@J�@_@��@�7@/@#�@�@�@�f@�@��@PH@�@�@��@��@P�@S@��@��@��@��@��@��@�A@~�@i�@Q@O@��@�>@��@s�@Q�@4@#�@V@�@��@��@m�@[�@N�@�@��@�@�0@�P@F�@=@+@��@��@�x@Q@{@�@��@f�@?}@8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�3�A�9�A�?�A�HA�A�A�@�A�A�A�C�A�C�A�B�A�?HA�B�A�D�A�7A�IA���A��QA��A���A��A���A�  A���A��rA��A��A�uA���A��A��yA��/A��jA� �A�ѷA�HA�{A�	7A�HAў�A�G�A�\�A�`BA�*�A�� A�N�A�"�AƸRAçRA��)A�Z�A�MA�	A���A��A���A��jA��'A�3�A�;�A�X�A���A���A�<6A�[�A�`vA���A���A�:�A���A�V�A�n�A��hA���A�;0A�m]A���A���A���A��fA���A���A�{A�4�A���A|�$Az4At+Aq��Ao`�Ak_Ag�WAd_A`9�A^%FA\��AY��AX�AUtTAR��AO[WAL�AI�TAI�AH,�AF��ADa|AB_A?�A=VmA<+A9�9A6i�A3��A1��A0&�A/�A.��A-{�A,��A-"hA-��A,�A+�A*�BA*1�A)�A)�[A)!-A(x�A(�A'Z�A&�HA& iA%�A%��A%~�A$�&A$&�A"H�A ȴA �+A OA��A=A�A�:A(�A=�A��A��A��A�rAB[AO�A��A\�A��AݘA��A��A�AB�A�Al�A~A�A�A�A�[AL�AOA�Am�A�DAT�A��AU�A?A�A��Ag8A��A��AffAOvA�mA��A�WA��A�A��ATaA�A1�A7A��A:*A9XA�AoiA��A��A
�A
�rA
?A
�A	�A	��A	N<A	A��AMA��A7�AaA˒A<�A��AC-AϫA�AtTA+A �mA ��A zA !-@��*@���@�S�@��b@��d@�m]@�N<@��	@��O@�/�@�ԕ@���@��@���@��6@���@�J#@�b�@�;@�<�@�%@��*@�a@�  @�i�@�H�@���@�K�@�@�C@���@�H@���@�O�@���@��r@�Z@�C�@�Q�@�7@�j@�@@�kQ@�c�@�s�@�<6@���@�{J@��f@�W�@��@�=@��@��@�\�@�8�@�W�@�r@⤩@�\@��@��@��@�{@߲�@��@�\�@�;@�m]@߯�@ߗ�@�x�@�RT@��@��8@���@���@���@�j@���@�Y�@���@܎�@�c @�
�@�zx@�@�}V@�!@� \@�-�@�u%@��P@�u%@�D�@�%�@��T@ӝ�@�e�@�[W@�X�@���@�z�@�6@�2�@�G@��@Ѽ@��@��@ϣn@�V@�\�@�%�@�@�hs@��@̀�@�@�?}@��`@ʴ9@�|�@�}�@ȫ6@�Ov@ǲ-@�C�@�)_@ƯO@ŷ@���@��@�H@�@���@���@�5?@�;�@��j@�2a@��@���@��+@��?@���@�ϫ@���@�v`@�T�@��/@��I@�ff@�@�@�@��W@�tT@�iD@�(@�Ɇ@�H�@�dZ@��@��D@��@���@�X�@�@@��j@�h�@�1'@�{@��Z@��@��@�P�@��f@���@��@�9X@�	�@���@��)@���@�U�@��@���@�C-@��K@���@��	@���@�~�@�[�@��@�P�@��|@�H�@��N@���@�Y�@�&�@��s@�R�@�˒@���@�S&@�&@��@�ی@���@��+@�4n@�� @���@��@�e,@�(�@��@���@��@�W�@���@���@�W?@��@�q�@���@���@�iD@��@���@��o@��9@�y�@�+�@���@�r�@�Ta@��>@��t@�n/@�>�@�@@���@�YK@�@��^@�`B@�4�@��f@���@��D@��@���@���@�9�@��|@���@�c�@�?@�'R@�	@�G@�@��@�Q�@���@��@��A@���@�"�@��@��@��b@�h
@��@���@��@�ԕ@��@�F@��X@���@�H�@�=q@�!@�J@���@��@���@�2a@��@���@��m@���@�e@���@�o @��"@��b@�Z@�?�@�6�@�
�@��@�A�@��$@�<�@��.@��>@��K@��"@��@��e@�m�@�'R@���@�|@�J�@�7L@�6z@�)_@��@��y@���@��\@�Ft@��D@��6@��H@���@���@�w2@�P�@��@�ߤ@�n�@��o@���@�J�@�;@��9@��@�m�@�GE@�:�@�&�@�x@��@���@��d@�x@�ȴ@�n�@�0U@���@���@�Mj@���@�Ɇ@���@�{�@�g8@�E�@�@�|�@��@�ی@���@��o@�;�@�"h@�r@�a@(@}�9@}f�@|��@|S�@{��@z�M@z�@z@�@y��@y/@y@@x�?@w��@wE9@w�@v��@vl�@v8�@vu@ua�@t<�@sC�@r�s@r�x@r^5@r	@qhs@pی@pw�@pA�@oH�@n��@n�X@n6�@mzx@mrG@mf�@m2a@l�o@k�@k��@k�P@ka@j�8@j҉@j��@j0U@i�H@i:�@h�j@hM@g�r@g�g@g�@g�:@gY@f��@f3�@e�z@d��@d:�@c�@c��@co�@b��@bE�@b!�@be@a��@a^�@`��@`-�@_�:@_6z@^��@^E�@]��@]%@\oi@\M@[�0@[;d@Z�"@Z��@Z��@ZB[@Y�@Y|@Y/@X�U@X`�@X(�@Xb@W�@W8@V��@VH�@U��@Ux�@T�|@Te�@T2�@S�A@S��@SdZ@SX�@S@R�A@Q�o@Q�@Q<6@QV@P�@P>B@P'R@P�@O�A@O�a@OZ�@N��@Na|@N#:@Mϫ@M|@ML�@M@L��@LQ�@K��@K�	@KC�@J�\@JGE@I�@Ij@H�j@H��@H~(@HQ�@H@H�@G��@G6z@Fc @F&�@E�T@Eo @D�e@DI�@D2�@D�@C��@C�@B��@Bc @A�T@A\�@@�@@j@?�@?qv@?P�@?@>kQ@>	@=ϫ@=�@=A @=(�@<�5@<Xy@;�a@;~�@:�@:��@:�A@:�@9�S@90�@8��@8�@8@7��@7��@7�V@7\)@7"�@7�@7�@6�c@6�B@6��@5�Z@5w2@5<6@4�`@4��@4�D@4j@4D�@4M@3��@3��@3�	@3X�@3�@2i�@2.�@24@2_@1��@1�@1�@1��@1�t@1c@0�5@07�@/�;@/@O@.��@.Ta@.J@-�@-�C@-8�@,�P@,�E@,j@,:�@+�@+��@+O@+�@*�@*��@*H�@)�T@)�-@)�S@)s�@)\�@)B�@)@(�$@(�o@(>B@(b@'��@'��@'v`@&�"@&\�@&3�@&8�@&J@%�@%��@%2a@$�@$��@$z�@$N�@$�@#�k@#�@#x@#o�@#E9@"�2@"��@"��@"s�@"W�@"#:@" �@!�)@!�@!�j@!�3@!�=@!4@!;@ �@ ی@ �9@ ��@ w�@ @� @�6@g�@9�@�@�M@�x@kQ@E�@?@3�@u@��@�@�@Vm@��@�[@Xy@�@��@~�@.I@��@�<@��@#:@��@�>@��@��@�)@�@s�@�@��@�e@��@z�@`�@@�+@�W@�;@��@4�@o@�@�@��@�2@��@�!@��@J�@_@��@�7@/@#�@�@�@�f@�@��@PH@�@�@��@��@P�@S@��@��@��@��@��@��@�A@~�@i�@Q@O@��@�>@��@s�@Q�@4@#�@V@�@��@��@m�@[�@N�@�@��@�@�0@�P@F�@=@+@��@��@�x@Q@{@�@��@f�@?}@8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	=B		B	=B	qB	�B	�B	�B	qB	�B	�B	�B	�B	�B	�B	B	"4B	'�B	./B	7fB	>BB	@�B	D�B	R B	]IB	e�B	iDB	l�B	q�B	u�B	}"B	�B	��B	��B	��B	�VB	sMB	t9B	t�B	qB	n�B	m�B	iB	h
B	h
B	v+B	��B	յB	�JB
�B
�B
+�B
2B
d�B
��B	�B BB(�B4�B9$B4�B�BaB)B�B
��B
��B
��B
�}B
�B
�NB
�dB
��B
��B
��B
��B
�nB
�TB
͹B
�|B
��B
j�B
TaB
5tB
oB	��B	��B	��B	�B	��B	}�B	cB	N�B	C�B	?B	5?B	,"B	!B	NB	�B�	B�GB��B�B�B�B�WB�EB�uBϑBʦBȴB��B�B��B	9B	,"B	6�B	A;B	^B	��B	�~B	�[B	�#B	��B	�B	��B	��B	�B	�B	�B	��B	�]B	�}B	��B	��B	��B	�VB	� B	�B	��B	�.B	�HB	��B	��B	�B	�B	߾B	یB	�1B	�B	�=B	�KB	�B	��B
gB
�B
{B
�B
pB
!|B
!�B
!|B
"hB
!�B
!B
!HB
#�B
$�B
%B
$�B
#�B
$�B
&B
&�B
#�B
$@B
%�B
%`B
#TB
"�B
VB
�B
?B
yB
$�B
.IB
5B
6B
6�B
6�B
6�B
4�B
6�B
8lB
6+B
2�B
4�B
9rB
9�B
:^B
9rB
:B
;B
;�B
;JB
:�B
:�B
9�B
9$B
8�B
6`B
5tB
4B
/ B
*KB
%�B
"hB
�B
�B
�B
�B
 B
dB
)B
�B
B
B
�B
xB
�B
9B
�B
�B
�B
�B
�B
mB
�B
B
B
	B
[B
�B
 �B
�B
 vB
�B
�B
=B
�B
qB
�B
@B
oB
B

B
�B
"�B
"NB
�B
�B
IB
]B
]B
jB
!B
pB
pB
jB
�B
�B
�B	�qB	��B	�VB	��B	��B	��B	�B	��B	�B	��B	�<B
 B
�B
B
_B
SB
�B
�B
�B
YB
�B
)B
BB
�B
�B
�B
TB
�B
�B
�B
�B
B
�B
:B
NB
�B
HB
�B
�B
�B
�B
dB
xB
�B
B
xB
B
�B
%B
B
�B
�B
B
gB
3B
MB
aB
MB
�B

�B
6B
�B
B
�B
B
�B

�B

�B

rB
�B
pB
pB
�B
pB
pB
"B
�B
B
�B
�B
�B
�B
�B
PB
B
�B
JB

�B
�B
mB
�B
�B
1B
�B
B
�B
�B
�B
+B

	B
	RB
�B
�B
0B
dB
JB
�B
�B

�B

	B
�B
B
�B
�B
�B
�B
 �B
 �B
 4B
 �B
 �B
B
oB
�B
�B
�B
'B
'B
AB
GB
�B
�B
�B
�B
gB
gB
3B
�B
�B
%B
B
�B
�B
�B
�B
+B
EB
�B
�B
1B
B
	�B
	�B

	B

=B

XB

�B
)B
�B
�B
B
dB
dB
~B
~B
�B
6B
�B
�B
B
<B
�B
�B
HB
}B
�B
�B
�B
uB
uB
B
[B
�B
B
@B
@B
�B
aB
�B
�B
gB
�B
�B
�B
�B
�B
�B
�B
B
eB
�B
	B
kB
B
B
�B
�B
kB
�B
#B
WB
B
)B
CB
xB
xB
xB
�B
�B
�B
dB
B
�B
�B
pB
�B
 BB
 BB
 'B
 \B
!bB
!bB
!HB
!HB
!|B
"hB
#B
#�B
#nB
#:B
"�B
"�B
#nB
$B
$�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
&fB
&�B
'�B
(>B
'�B
'�B
'�B
'�B
(�B
)_B
*eB
*eB
*eB
*eB
*0B
*0B
*B
*eB
*�B
+QB
+kB
+�B
+�B
+�B
+�B
+�B
,"B
,�B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
./B
.�B
/ B
0!B
0;B
/�B
1AB
1�B
1�B
2�B
2�B
2�B
3B
3MB
3hB
33B
2�B
33B
49B
4B
4nB
4�B
4�B
5B
5?B
5�B
5�B
5�B
5�B
6B
6FB
6�B
7B
6�B
6zB
6�B
7�B
7LB
7�B
7�B
8�B
9	B
9XB
9�B
9�B
:B
:*B
:DB
:DB
:xB
:�B
:�B
:�B
;�B
<B
<B
<PB
<PB
<6B
<B
<B
<�B
=B
="B
="B
=qB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>wB
>�B
?cB
?cB
?HB
?.B
?�B
@B
@OB
@OB
@OB
@�B
@�B
@�B
@�B
A B
A�B
A�B
BAB
B�B
B�B
BuB
B�B
B�B
C{B
C�B
C�B
D�B
D�B
D�B
EB
EB
E�B
E�B
FB
E�B
E�B
FtB
F�B
F�B
GzB
G_B
GzB
HB
HKB
IB
IRB
IlB
I�B
I�B
I�B
I�B
J	B
JXB
J�B
J�B
J�B
J�B
K)B
KDB
KDB
K)B
K�B
LB
LJB
L~B
L�B
MB
MjB
MjB
M�B
NpB
NpB
NVB
NpB
OB
OBB
OvB
O�B
O�B
O�B
PHB
PHB
PHB
PHB
PHB
P}B
P�B
PbB
P�B
Q B
QNB
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
S�B
S�B
TB
T{B
T�B
T�B
T�B
T�B
U2B
UB
U2B
U�B
VSB
VmB
VmB
V�B
V�B
W
B
W$B
W�B
W�B
XB
W�B
XEB
X�B
YKB
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\B
\CB
\B
\CB
\xB
\�B
]B
\�B
\�B
\�B
[�B
[WB
[=B
[�B
\]B
\]B
\xB
\]B
]dB
^5B
^�B
^�B
^�B
^�B
_B
_;B
_�B
`�B
`�B
a-B
a|B
a|B
a�B
a�B
bB
bB
bNB
bhB
b�B
b�B
c B
c B
c:B
c:B
c:B
c:B
c:B
c B
c:B
c B
c B
c:B
d&B
dZB
d�B
eB
e,B
eFB
ezB
fB
f2B
f2B
f�B
f�B
g8B
gRB
g�B
g�B
g�B
h>B
h�B
i*B
iDB
i_B
i_B
iyB
i�B
i�B
jKB
jeB
j�B
j�B
j�B
kB
kB
kQB
j�B
j�B
k6B
kkB
kkB
k�B
lWB
lqB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
oB
oOB
oOB
oiB
oOB
o�B
o�B
pB
pB
pB
pB
pB
p!B
poB
poB
p�B
p�B
p�B
p�B
p�B
qvB
qvB
q�B
r-B
r|B
r|B
r|B
r|B
r�B
r�B
r�B
s3B
s3B
s�B
tB
s�B
tB
tB
tB
s�B
tB
t9B
t9B
tTB
tnB
t�B
u%B
u�B
utB
u�B
u�B
u�B
vB
u�B
vzB
wfB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y$B
y�B
y�B
zB
zDB
z^B
zxB
z�B
z�B
z�B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
{B
{JB
{JB
{JB
{JB
{0B
{dB
{B
{B
{�B
{�B
{�B
|B
|6B
|�B
|�B
}<B
}VB
}VB
}VB
}VB
}�B
}�B
}�B
~BB
~B
~BB
~wB
~wB
~�B
~�B
~�B
~�B
.B
�B
� B
� B
�4B
�iB
�OB
��B
��B
�OB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	=B		B	WB	�B	�B	�B	�B	qB	�B	�B	�B	�B	�B	�B	OB	"NB	'�B	./B	7fB	>BB	@�B	D�B	R B	]IB	e�B	iyB	l�B	q�B	u�B	~(B	��B	�>B	��B	�sB	�B	x�B	w�B	w2B	sB	p�B	p!B	jeB	jB	m�B	}VB	�`B	خB	�6B
�B
\B
-wB
5?B
i_B
�PB�B#�B-CB<6B@B=�B&�BdB\BKB
��B
�VBGBB
�jB
��B
�hB
��B
��B
��B
��B
��B
�?B
ӏB
��B
�B
o�B
ZB
:�B
�B
B	�SB	�MB	��B	�B	�'B	g�B	Q�B	E�B	BAB	8B	/�B	"�B	gB	tB�B�B�UB��B��B�B�OB�#B՛B��B��B��B�}B��B�`B	YB	-�B	7�B	@�B	^5B	��B	��B	�{B	��B	�BB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	�4B	��B	�EB	�}B	ңB	��B	��B	� B	�TB	��B	֡B	�FB	��B	�B	�]B	�B	�4B	�B	�B	�LB	�/B
�B
�B
�B
�B
�B
!�B
"NB
"B
"�B
!�B
!|B
"4B
$ZB
%�B
&2B
%`B
$�B
%�B
&�B
'�B
$&B
$ZB
&2B
%�B
#�B
#�B
 vB
�B
?B
B
$�B
.IB
5%B
6+B
7LB
7�B
7fB
5?B
6�B
8�B
6�B
2�B
5ZB
:B
:^B
:�B
:*B
:�B
;�B
<B
;�B
;dB
;dB
:^B
9�B
9XB
6�B
6FB
5B
/�B
+B
&�B
# B
;B
�B
7B
uB
uB
�B
^B
 B
�B
�B
&B
~B
_B
mB
�B
MB
B
B
�B
mB
�B
�B
�B
�B
B
�B
!-B
 'B
!-B
5B
�B
WB
 �B
�B
�B
�B
�B
�B
�B
jB
"�B
"�B
 'B
�B
IB
xB
xB
�B
!|B
�B
 B
�B
dB
@B
�B	��B	��B	��B	��B	��B	�<B	�6B	��B	��B	��B	�B
  B
'B
�B
�B
�B
B
�B
�B
tB
SB

�B
B
B
B
�B
oB
�B
hB
�B
�B
hB
 B
�B
�B
 B
}B
.B
BB
<B
6B
�B
0B
~B
B
JB
�B
�B
?B
?B
�B
�B
B
�B
gB
�B
{B
MB
�B

�B
jB
B
�B
�B
jB
B

�B

�B

�B
6B
�B
�B
B
�B
�B
VB
�B
pB
"B
�B
�B
�B
�B
�B
�B
B
�B
�B
?B
�B
B
�B
fB
	7B
+B
�B
�B
�B
zB

rB
	lB
�B
B
~B
�B
dB
0B
�B
DB

�B
�B
SB
�B
SB
�B
�B
;B
 �B
 �B
 �B
 �B
UB
�B
�B
�B
�B
AB
[B
uB
aB
�B
�B
�B
�B
�B
gB
MB
�B
�B
YB
?B
�B
�B
�B
�B
zB
zB
EB
�B
fB
�B

	B
	�B

=B

XB

�B

�B
^B
�B
�B
0B
~B
~B
�B
�B
�B
jB
�B
�B
<B
pB
�B
�B
bB
�B
�B
,B
�B
�B
�B
FB
�B
B
&B
uB
�B
B
�B
�B
B
�B
�B
B

B
�B
�B
+B
+B
_B
�B
�B
#B
�B
QB
7B
�B
�B
�B
�B
WB
�B
CB
CB
CB
xB
�B
�B
�B
B
B
�B
5B
B
�B
�B
 B
 BB
 BB
 \B
 �B
!bB
!|B
!bB
!bB
!�B
"�B
#:B
#�B
#nB
#TB
"�B
"�B
#�B
$@B
$�B
%�B
&fB
&�B
'B
'B
'B
'B
&�B
&�B
'�B
(>B
(
B
'�B
'�B
(
B
)DB
)�B
*�B
*B
*B
*�B
*B
*eB
*0B
*�B
*�B
+�B
+kB
+�B
+�B
+�B
+�B
+�B
,=B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-B
.IB
/B
/OB
0UB
0UB
/�B
1[B
1�B
1�B
2�B
2�B
2�B
3B
3hB
3�B
3hB
3MB
3�B
4TB
49B
4�B
4�B
4�B
5?B
5tB
5�B
5�B
5�B
6B
6FB
6�B
7B
72B
6�B
6�B
6�B
7�B
7LB
7�B
8B
9	B
9$B
9�B
9�B
:*B
:DB
:^B
:xB
:xB
:�B
:�B
:�B
;JB
;�B
<B
<B
<PB
<jB
<PB
<jB
<PB
<�B
="B
=<B
=<B
=�B
=�B
>B
>B
>B
>B
>�B
>�B
>�B
?.B
?cB
?}B
?HB
?cB
@ B
@4B
@iB
@OB
@iB
@�B
@�B
@�B
@�B
A;B
A�B
A�B
BAB
B�B
B�B
BuB
B�B
CB
C�B
C�B
C�B
D�B
EB
EB
EB
E9B
E�B
E�B
F%B
E�B
FB
F�B
F�B
GB
G�B
GzB
G�B
H1B
H�B
IB
IRB
I�B
I�B
J	B
J	B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
K)B
KDB
KDB
KxB
K�B
LJB
LdB
L�B
MB
M6B
MjB
MjB
M�B
NpB
N�B
NpB
N�B
O(B
O\B
O�B
O�B
O�B
O�B
PbB
PbB
PHB
PbB
PbB
P�B
P�B
P}B
P�B
Q B
QNB
Q�B
Q�B
R:B
R�B
R�B
R�B
R�B
S�B
S�B
TFB
T�B
T�B
T�B
UB
T�B
U2B
U2B
UgB
U�B
VmB
VmB
V�B
V�B
V�B
W$B
W$B
W�B
W�B
XEB
X+B
X_B
YB
YB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\B
\)B
\]B
\B
\]B
\�B
\�B
]/B
\�B
\�B
\�B
[�B
[qB
[=B
[�B
\�B
\xB
\xB
\]B
]~B
^5B
^�B
^�B
^�B
^�B
_!B
_VB
`B
`�B
`�B
a-B
a|B
a|B
a�B
a�B
b4B
b4B
bNB
bhB
b�B
b�B
c:B
c B
c:B
c:B
c:B
c:B
c:B
c:B
c:B
cTB
cTB
cnB
d@B
dtB
d�B
eB
eFB
eFB
e�B
f2B
f2B
fLB
f�B
f�B
g8B
gmB
g�B
g�B
g�B
h>B
h�B
i*B
i_B
iyB
iyB
iyB
i�B
i�B
jKB
jB
j�B
j�B
kB
kB
k6B
k�B
kB
j�B
kQB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
nB
ncB
n�B
n�B
n�B
n�B
o B
o5B
o5B
oOB
oOB
oiB
o�B
o�B
pB
o�B
o�B
p!B
p!B
p!B
poB
poB
p�B
p�B
p�B
p�B
p�B
qvB
qvB
q�B
r-B
r|B
r�B
r�B
r|B
r�B
r�B
r�B
sMB
shB
s�B
tB
tB
t9B
tB
tB
s�B
t9B
t9B
tB
tTB
tnB
t�B
u%B
u�B
utB
u�B
u�B
u�B
u�B
u�B
v�B
wfB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
yXB
y$B
y�B
y�B
z*B
zDB
zDB
z^B
z�B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{0B
{JB
{0B
{0B
{dB
{dB
{B
{�B
{�B
{�B
|6B
|PB
|�B
}B
}<B
}VB
}VB
}VB
}<B
}�B
}�B
}�B
~(B
~B
~BB
~wB
~wB
~wB
~�B
~�B
~�B
HB
�B
�B
�B
�4B
��B
�iB
��B
��B
�OB
��B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.1(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908020049102019080200491020190802004910202207271132032022072711320320220727113203202207271534412022072715344120220727153441  JA  ARFMdecpA30a                                                                20190722094509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190722094544  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190722094545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190722094545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190722094545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190722094546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190722094546                      G�O�G�O�G�O�                JA  ARUP                                                                        20190722100036                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190723000000  CF  PSAL_ADJUSTED_QC@��@��G�O�                JM  ARCAJMQC2.0                                                                 20190801154910  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190801154910  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023203  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063441  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                