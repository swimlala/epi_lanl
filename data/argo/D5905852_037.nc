CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-07T18:39:28Z creation;2020-02-07T18:39:30Z conversion to V3.1;2022-08-02T05:11:27Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200207183928  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA  A30_8420_037                    2C  D   APEX                            8420                            2.11.2                          846 @�$�^o�1   @�%9 @-���,�c�ᰉ�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�ffB�ffB���B�  B�  B�  B�  B���B�33B�ffB�  B�33B���B�  B�  B���B�ffC  C  C  C  C
  C  C  C  C  C  C  C�C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @E�@�p�@��A z�A!p�AAp�AaG�A���A��A�
=A���A���A��A�G�A���B �B\)B�\B\)B \)B(�\B0z�B8z�B@�\BH��BP�BX��B`�Bh\)Bp\)Bx�
B�W
B�#�B�#�B�B�B�B�B�B�B�=qB�8RB�=qB�=qB�8RB�=qB�\)B�aHB�B��B��=BøRB�\B�G�B�G�B�.B�(�B��B��B�B�=qB�aHB��B�8RB�G�B�  B���C{C�C
C�C
#�C#�C!HC!HC!HC�C�C33C&fC�C�C 
C"�C$#�C&#�C()C*)C,�C.�C0�C2
C4�C6�C8�C:{C<
C>�C@�CB@ CD�CF{CH�CJ
CL)CN)CP{CR
CT)CV)CX�CZ!HC\!HC^�C`�Cb)Cd)Cf)Ch�Cj�Cl!HCn!HCp�Cr�Ct�Cv)Cx{Cz{C|)C~!HC��C��C�\C��C��C�C��C�C��C��C�\C��C�3C�\C��C��C��C��C��C�{C�C�
=C��C��C��C��C�3C��C��C��C�C�\C��C�\C�\C��C��C�C��C�\C�\C�C��C�\C�3C��C��C�\C�\C�\C��C��C��C��C��C�C�\C�C��C��C��C��C�{C�{C��C��C�\C�\C��C��C��C�
=C��C��C��C��C��C�\C�C��C��C��C�C�C�C�\C�\C��C�\C�\C��C��C��C�3C��C�\C��C��C��C�\C�3C��C�C��C�3C�3C�C�
=C�
=C�
=C�
=C�
=C��C��C�\C��C��C�RC�\C�\C��C��C�C��C�\C�C�C�\D �D �
DRD�RDfD��D�D��DRD��D
D�
D�D�RD�D��D�D�fD	RD	�
D
fD
��D�D��D�D�RD�D�
DD��DRD�
DfD�
DfD��D�D�RDRD�RD�D��D
=D�RD
D��D
D�DfD�RD	�D��D
=D��D�D��DfD�fDRD�=DRD�=D)D��D 
D �RD!
D!�RD"RD"�RD#�D#�fD$fD$�fD%
D%�fD&RD&�RD'
D'�RD(	�D(��D)�D)�RD*RD*�
D+RD+�RD,�D,��D-
D-�
D.fD.��D/RD/�
D0
D0��D1fD1��D2
D2��D3RD3��D4RD4�
D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:
D:�RD;�D;�
D<�D<��D=�D=��D>
D>��D?�D?�
D@fD@��DAfDA��DB�DB�fDC�DC�
DD
DD�RDE	�DE��DFfDF�DG�DG�fDH
DH��DI�DI��DJ�DJ�RDK�DK�RDLRDL��DMRDM�RDN�DN��DO�DO�
DP
DP��DQRDQ��DRRDR��DS�DS��DTfDT�
DU�DU�fDVfDV��DWfDW�
DX�DX�RDY	�DY��DZfDZ�RD[�D[�fD\�D\��D]
D]��D^
D^�fD_fD_�fD`
D`�
Da
Da�RDbRDb�RDcRDc��Dd�Dd��DeRDe��Df�Df�
DgDg��DhfDh�
Di�Di��Dj
Dj�
Dk
Dk��Dl�Dl��Dm
Dm�
DnRDn��Do�Do�fDp
Dp��Dq�Dq�
Dr�Dr�fDs�Ds��DtRDt��Du	�Du��Dv	�Dv�
DwDw�fDxRDx�RDy�Dy�
DzRDz��D{
=D{��D|
=D|��D}�D}�fD~
D~�RDRD�
D��D�C3D��)D��{D��D�D)D��)D�ÅD�3D�C�D��{D���D��D�D)D���D��)D��D�B�D���D��3D��D�C�D���D�ÅD�)D�C�D���D���D��D�C3D��)D��{D��D�C�D��3D���D�3D�C3D���D��)D��D�D{D���D���D��D�B�D���D�ÅD��D�B�D���D��3D��D�C�D��)D��)D�)D�C�D���D��3D��D�ED��qD���D�D�ED���D��3D�{D�D{D���D���D�)D�ED��{D���D�D�C�D���D�ÅD�{D�ED��{D��)D�)D�C�D���D��)D�)D�C�D���D�ÅD�3D�C3D��{D��)D��D�C�D���D���D��D�C�D���D�ÅD�)D�C�D��)D��)D��D�C�D���D��)D�{D�D{D���D�ÅD��D�D{D��{D�ÅD�)D�C�D���D���D��D�D{D��{D��{D��D�C�D���D��)D�3D�C3D��{D��{D�{D�C�D��3D��{D��D�C�D���D��)D��D�C�D���D��D��D�B�D���D���D�{D�C�D���D���D��D�C3D��3D�ÅD��D�C�D��)D���D��D�D)D��3D��)D��D�D{D��)D��)D�3D�B�D���D���D��D�C�D��D���D��D�EqD��)D�ÅD�)D�C�D��)D��)D�{D�D{D���D��)D�)D�C3D���D��3D��D�D{D��{D�ÅD��D�ED��{D��)D�)D�D)D��{D���D��D�D)D���D���D�)D�B�D��3D��)D�{D�D�D��D���D�{D�D�D��{D��)D�)D�D�D���D���D��D�C�D���D��{D��D�C3D��3D��3D��D�C�D���D��)D�)D�C�D���D���D�)D�B�D��3D��)D�)D�C�D���D��{D�)D�C�D���D���D��D�D�D���D��qD��D�C3D��3D���D��D�C�D)D��)D�)D�D)DÄ{D��)D��D�B�Dă�D���D�)D�D�DŃ�D���D��D�C�Dƃ3D��3D�)D�D)Dǃ�D���D��D�C�DȄ{D�ÅD�3D�C3DɄ{D��{D��D�C3Dʃ3D���D��D�B�D˃�D���D��D�D�D̄{D��{D�)D�D)D̈́�D�ÅD��D�C�D΄{D���D��D�C�Dσ3D���D��D�C3DЃ�D�ÅD��D�C�Dу�D�ÅD��D�C�D҄)D��{D�{D�D)DӃ�D�ÅD�3D�D)Dԃ�D�ÅD�3D�B�DՃ3D�ÅD��D�D{Dփ3D��3D��D�C�Dׄ)D��{D�{D�C�D؃�D���D�)D�C�Dق�D�D��D�C�Dڃ�D���D�)D�D�Dۄ)D��3D��D�D{D܃�D���D��D�C�D݃�D��)D�D�EqDބ{D�ÅD��D�D{D߅D��qD��D�D)D��{D���D��D�B�DႏD��=D�=D�C3D�{D��{D��D�C�D�=D���D��D�B�D䃅D��{D�D�D�D��D��D�{D�D)D��D��)D�{D�D{D�{D��)D�{D�D{D�{D�ÅD��D�C3D�3D���D�3D�B�D��D���D�)D�C�D�3D��3D��D�D{D�qD��)D�3D�C3D탅D���D�{D�D{D�3D�D��D�D�D�)D���D��D�C�D���D���D��D�D{D��D�ÅD�)D�C3D�3D���D��D�C�D�D��3D�3D�C�D�D���D��D�C�D���D���D��D�D{D��)D���D�{D�C3D���D���D�{D�C�D���D��{D�{D�C3D���D���D��D�D)D���D���D�{D�EqD��{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�'RA�,A�1'A�=<A�?A�EA�@A�>wA�=�A�EA�HA�I�A�GzA�B[A�GEA�FA�LdA�N�A�O�A�S[A�T�A�UgA�W�A�XyA�Y�A�[WA�XyA�J�A�E�A�<�A�<�A�@A�1�A�'�A��A�A�oA�PA�	�A��A�A���A���A���A�ѷA�^�A�K^A�x�A��jA��A�y�A�uA�sA�A�A�1'A�
	A�,�A�$A�]dA��A�2-A�w�A�X�A��@A���A� �A� 'A�aHA��A�(A~ɆA|XyAy�fAwRTArS�Am�MAg�Ac#:A]�A\+kA[=�AZ��AYtTAT�>AP8AK�mAF��AD�	ADt�AB33A?�A>�A<�YA8�]A6��A4�/A2�	A0�AA/Q�A.4�A,�5A+�A)�	A( iA&  A% �A%��A$iDA"�A �`A�A�AS�AG�AVA(�A��A@�Aa|AD�A�A�*A(�AE9AF�A	A��A&�A�,A�hA�rAM�A�-A�AOvA��A�hA�\AT�A4A҉Al�A�A�MA)_A�)A~�A�}AHA-A��A�9A��A�{A:*A�#AhsA�A�WA�vAW?A�\AjA0�A�FAU2AL0A6A�XA\�A�A�A�cA�A
�nA
IRA	�FA	q�A	1�A	�A�2A��AA A��A��ATaA�-A(�A�xA \A��A1�A%A��A9XAA�jA��AGEAѷAs�A �&A ˒A ��A ĜA %@��=@�s�@�e,@���@��@�c @���@��@���@��{@��@�/�@���@���@��@�T�@���@�'R@��]@��@���@��@��@�d�@�,=@�7@��@�w�@��@�Vm@���@�@�@��#@�%@�H�@��@�"�@��`@貖@�D�@畁@�X�@���@�m�@��@�tT@��@��T@�;d@��@�YK@�j�@�7@�?}@�D�@�:�@�y>@ٺ^@�U�@���@ؒ�@�bN@�G@׮@�x@ֿ�@���@�33@Ԥ�@�z�@��@ӍP@��@һ�@�H@���@�-w@�oi@��Z@�v`@�@Υz@��@��
@��@�M@��@˛=@�]�@�33@��@���@�M@���@�k�@�;d@��@ȫ6@� �@�ݘ@Ǔ@�G�@�ȴ@ƫ6@�L0@���@�˒@�W?@�͟@�B[@î�@�=�@¹$@Y@�/�@���@�RT@��`@�7�@��@�x@�`B@�1�@��,@�M@���@���@���@�X@�$t@��m@���@�v`@�=�@�ߤ@�C�@��#@��	@��@�Q�@�:*@���@�9�@��@��p@��@�q@�!@��w@�=�@��@���@��_@�?@��T@�@�[W@��@�[�@��@��W@���@��=@���@���@��M@�c@��6@�^5@���@��-@�j�@�%F@��@���@��R@��,@��@�hs@���@�!-@���@�˒@���@��h@�`B@��K@��r@�c�@�s�@��>@���@��@���@�g8@�\�@�$�@�ݘ@���@�U�@�H�@��[@�-�@��V@�J#@��@��@��}@�s�@��@�_@�7@���@�rG@�\)@�@O@� i@�Ĝ@���@�kQ@��z@�(�@���@�K^@���@�?}@��8@�֡@��@�Ta@��@���@�v`@�Vm@��|@���@�n�@�A�@�'R@��@���@��X@�x�@�:�@��@��@��U@��Y@�Ta@�O@��m@��~@�@�|�@�,=@�J@���@��@��g@��F@��V@�y�@��@���@�Z�@�@��o@��>@���@�T�@���@��s@�_�@��@�O�@�6z@�.I@���@��R@�s�@�3�@��@��@��`@��u@�W�@�?�@�O@��r@���@�o�@�1�@��@�]d@�g8@�7�@�@���@��@���@�s�@��@���@���@�m�@�Ta@�3�@��]@���@��S@��4@�y�@�e�@��@��@��F@�}V@�J�@�O@�G@��Q@��-@���@���@�zx@�Dg@��@�V@���@��@��[@�^5@��D@��S@�.I@� i@�Ɇ@��r@�N�@�˒@���@�}�@��@�j@�4n@��@���@��#@��4@��@��U@�w�@�,=@��N@���@�W?@�/@�	l@���@��L@�'R@��]@��H@��	@�Z�@�C@���@�r�@�"h@�	@��@P�@~^5@~�@}�o@}�@}ϫ@}�z@}�@}|@}�@|��@|j@{�@@{C�@{�@z��@za|@zJ@y�@y��@ye,@y!�@x��@x9X@w�@wS@v�@vW�@v5?@u��@t�@t�Y@tx@sƨ@s�{@sA�@r�@q�M@qVm@qIR@p�f@p�.@pm�@p>B@p  @o��@o�*@o�P@n��@n�\@nQ@mԕ@l�P@l֡@l��@lN�@k�&@k�@k>�@j��@j_�@j@i��@i�@i[W@h�@hj@h!@g��@f�c@fR�@e�@e|@e4@d�@d��@d��@dD�@c�@c��@c,�@bd�@a�@a�'@aJ�@`�	@_ݘ@_��@_�[@_�q@_��@_n/@^�8@^�'@^��@^C�@]��@]}�@]=�@]2a@]�@\��@\֡@\h�@\�@[�}@[Z�@Z�A@Z0U@Y��@Y��@YS&@X��@X-�@W˒@WdZ@V��@V�H@V҉@VGE@U�#@U��@U=�@U%F@Tی@TI�@T�@T	�@T  @S�W@S�m@S��@S&@R�H@RGE@Q�@Q�n@Q�@Q4@P��@P1'@O�Q@O�@@OF�@N� @N_�@N+k@MrG@L��@L�@LbN@Lx@Kt�@KS@J��@J�@JW�@J{@I��@H�@H�@Hj@H(�@G��@G��@G��@Ga@G�@F��@F!�@E�@Ek�@D��@D�@C�}@C�:@Ct�@C6z@B�"@B�x@B{@A�^@A��@Ahs@A=�@A!�@A�@@�|@@��@@%�@@  @?�+@?�&@?��@?��@?��@?s@>�@>��@>6�@=��@=�@=�"@=Vm@=?}@= \@<�`@<�p@<�?@<�O@<�.@<g8@<Ft@<�@;�g@;�[@;��@;+@:q�@9��@9��@9�"@9p�@9Y�@9F@9�@8�[@8�$@8��@8N�@7�]@7��@7)_@7
=@6��@6�@6_�@5�Z@5��@58�@4�j@4_@4 �@3��@3�V@3�@2��@2�6@2}V@2O@1�@1�S@1%F@0��@0�_@0�o@0j@0�@/��@/�k@/��@/C@.�,@.�F@.h
@.\�@.=q@-�.@-��@-S&@-�@,�[@,�4@,�@,Ft@+�A@+��@+e�@*�s@*u%@*#:@)�>@)�z@)��@)N<@)@(��@(�e@(`�@(A�@'�]@'��@'E9@' i@&�R@&��@&;�@& �@%��@%��@%�H@%��@%%F@$�@$~(@$<�@$G@#��@#��@#@O@"��@"�@"�h@"�+@"0U@!��@!��@!/@ �	@ �@ ��@ ~(@   @� @��@�$@b�@ i@��@�F@_�@4@�@��@f�@֡@��@/�@��@��@/�@��@ں@��@��@Ta@�@ϫ@�7@IR@�@I�@�@��@�P@y�@O@9�@
=@�2@��@��@p;@GE@{@�@��@��@c@+@֡@�9@r�@*�@�+@��@+@�H@��@�!@�@�@kQ@Q@)�@�@��@��@m]@X@#�@��@��@u�@1'@�@��@�@a@)_@�@�@ȴ@��@��@�@n�@GE@�@�@��@|@m]@S&@q@��@�Y@?�@ �@�@�@�]@��@�
@�@@��@iD@H�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�'RA�,A�1'A�=<A�?A�EA�@A�>wA�=�A�EA�HA�I�A�GzA�B[A�GEA�FA�LdA�N�A�O�A�S[A�T�A�UgA�W�A�XyA�Y�A�[WA�XyA�J�A�E�A�<�A�<�A�@A�1�A�'�A��A�A�oA�PA�	�A��A�A���A���A���A�ѷA�^�A�K^A�x�A��jA��A�y�A�uA�sA�A�A�1'A�
	A�,�A�$A�]dA��A�2-A�w�A�X�A��@A���A� �A� 'A�aHA��A�(A~ɆA|XyAy�fAwRTArS�Am�MAg�Ac#:A]�A\+kA[=�AZ��AYtTAT�>AP8AK�mAF��AD�	ADt�AB33A?�A>�A<�YA8�]A6��A4�/A2�	A0�AA/Q�A.4�A,�5A+�A)�	A( iA&  A% �A%��A$iDA"�A �`A�A�AS�AG�AVA(�A��A@�Aa|AD�A�A�*A(�AE9AF�A	A��A&�A�,A�hA�rAM�A�-A�AOvA��A�hA�\AT�A4A҉Al�A�A�MA)_A�)A~�A�}AHA-A��A�9A��A�{A:*A�#AhsA�A�WA�vAW?A�\AjA0�A�FAU2AL0A6A�XA\�A�A�A�cA�A
�nA
IRA	�FA	q�A	1�A	�A�2A��AA A��A��ATaA�-A(�A�xA \A��A1�A%A��A9XAA�jA��AGEAѷAs�A �&A ˒A ��A ĜA %@��=@�s�@�e,@���@��@�c @���@��@���@��{@��@�/�@���@���@��@�T�@���@�'R@��]@��@���@��@��@�d�@�,=@�7@��@�w�@��@�Vm@���@�@�@��#@�%@�H�@��@�"�@��`@貖@�D�@畁@�X�@���@�m�@��@�tT@��@��T@�;d@��@�YK@�j�@�7@�?}@�D�@�:�@�y>@ٺ^@�U�@���@ؒ�@�bN@�G@׮@�x@ֿ�@���@�33@Ԥ�@�z�@��@ӍP@��@һ�@�H@���@�-w@�oi@��Z@�v`@�@Υz@��@��
@��@�M@��@˛=@�]�@�33@��@���@�M@���@�k�@�;d@��@ȫ6@� �@�ݘ@Ǔ@�G�@�ȴ@ƫ6@�L0@���@�˒@�W?@�͟@�B[@î�@�=�@¹$@Y@�/�@���@�RT@��`@�7�@��@�x@�`B@�1�@��,@�M@���@���@���@�X@�$t@��m@���@�v`@�=�@�ߤ@�C�@��#@��	@��@�Q�@�:*@���@�9�@��@��p@��@�q@�!@��w@�=�@��@���@��_@�?@��T@�@�[W@��@�[�@��@��W@���@��=@���@���@��M@�c@��6@�^5@���@��-@�j�@�%F@��@���@��R@��,@��@�hs@���@�!-@���@�˒@���@��h@�`B@��K@��r@�c�@�s�@��>@���@��@���@�g8@�\�@�$�@�ݘ@���@�U�@�H�@��[@�-�@��V@�J#@��@��@��}@�s�@��@�_@�7@���@�rG@�\)@�@O@� i@�Ĝ@���@�kQ@��z@�(�@���@�K^@���@�?}@��8@�֡@��@�Ta@��@���@�v`@�Vm@��|@���@�n�@�A�@�'R@��@���@��X@�x�@�:�@��@��@��U@��Y@�Ta@�O@��m@��~@�@�|�@�,=@�J@���@��@��g@��F@��V@�y�@��@���@�Z�@�@��o@��>@���@�T�@���@��s@�_�@��@�O�@�6z@�.I@���@��R@�s�@�3�@��@��@��`@��u@�W�@�?�@�O@��r@���@�o�@�1�@��@�]d@�g8@�7�@�@���@��@���@�s�@��@���@���@�m�@�Ta@�3�@��]@���@��S@��4@�y�@�e�@��@��@��F@�}V@�J�@�O@�G@��Q@��-@���@���@�zx@�Dg@��@�V@���@��@��[@�^5@��D@��S@�.I@� i@�Ɇ@��r@�N�@�˒@���@�}�@��@�j@�4n@��@���@��#@��4@��@��U@�w�@�,=@��N@���@�W?@�/@�	l@���@��L@�'R@��]@��H@��	@�Z�@�C@���@�r�@�"h@�	@��@P�@~^5@~�@}�o@}�@}ϫ@}�z@}�@}|@}�@|��@|j@{�@@{C�@{�@z��@za|@zJ@y�@y��@ye,@y!�@x��@x9X@w�@wS@v�@vW�@v5?@u��@t�@t�Y@tx@sƨ@s�{@sA�@r�@q�M@qVm@qIR@p�f@p�.@pm�@p>B@p  @o��@o�*@o�P@n��@n�\@nQ@mԕ@l�P@l֡@l��@lN�@k�&@k�@k>�@j��@j_�@j@i��@i�@i[W@h�@hj@h!@g��@f�c@fR�@e�@e|@e4@d�@d��@d��@dD�@c�@c��@c,�@bd�@a�@a�'@aJ�@`�	@_ݘ@_��@_�[@_�q@_��@_n/@^�8@^�'@^��@^C�@]��@]}�@]=�@]2a@]�@\��@\֡@\h�@\�@[�}@[Z�@Z�A@Z0U@Y��@Y��@YS&@X��@X-�@W˒@WdZ@V��@V�H@V҉@VGE@U�#@U��@U=�@U%F@Tی@TI�@T�@T	�@T  @S�W@S�m@S��@S&@R�H@RGE@Q�@Q�n@Q�@Q4@P��@P1'@O�Q@O�@@OF�@N� @N_�@N+k@MrG@L��@L�@LbN@Lx@Kt�@KS@J��@J�@JW�@J{@I��@H�@H�@Hj@H(�@G��@G��@G��@Ga@G�@F��@F!�@E�@Ek�@D��@D�@C�}@C�:@Ct�@C6z@B�"@B�x@B{@A�^@A��@Ahs@A=�@A!�@A�@@�|@@��@@%�@@  @?�+@?�&@?��@?��@?��@?s@>�@>��@>6�@=��@=�@=�"@=Vm@=?}@= \@<�`@<�p@<�?@<�O@<�.@<g8@<Ft@<�@;�g@;�[@;��@;+@:q�@9��@9��@9�"@9p�@9Y�@9F@9�@8�[@8�$@8��@8N�@7�]@7��@7)_@7
=@6��@6�@6_�@5�Z@5��@58�@4�j@4_@4 �@3��@3�V@3�@2��@2�6@2}V@2O@1�@1�S@1%F@0��@0�_@0�o@0j@0�@/��@/�k@/��@/C@.�,@.�F@.h
@.\�@.=q@-�.@-��@-S&@-�@,�[@,�4@,�@,Ft@+�A@+��@+e�@*�s@*u%@*#:@)�>@)�z@)��@)N<@)@(��@(�e@(`�@(A�@'�]@'��@'E9@' i@&�R@&��@&;�@& �@%��@%��@%�H@%��@%%F@$�@$~(@$<�@$G@#��@#��@#@O@"��@"�@"�h@"�+@"0U@!��@!��@!/@ �	@ �@ ��@ ~(@   @� @��@�$@b�@ i@��@�F@_�@4@�@��@f�@֡@��@/�@��@��@/�@��@ں@��@��@Ta@�@ϫ@�7@IR@�@I�@�@��@�P@y�@O@9�@
=@�2@��@��@p;@GE@{@�@��@��@c@+@֡@�9@r�@*�@�+@��@+@�H@��@�!@�@�@kQ@Q@)�@�@��@��@m]@X@#�@��@��@u�@1'@�@��@�@a@)_@�@�@ȴ@��@��@�@n�@GE@�@�@��@|@m]@S&@q@��@�Y@?�@ �@�@�@�]@��@�
@�@@��@iD@H�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�JB�JB�0B��B��B��B��B��B��B�xB��B��B��B��B��B��B��B��B��B��B�xB�xB�xB��B��B��B�xB��B��B��B��B��B�B�fB��B�`B�+B��B��B��B��B��B�B��B�qBC-B	6�B	�B
9$B
m�B
��B
��B
�B
`'B
,�B
)�B	��B	�B	��B	��B	��B
 �B	�6B	�B	��B	��B	�ZB	�gB	��B	��B	�B	��B	��B	y�B	j�B	S�B	9rB	'B	NB		�B	�B	{B��B�B��B�7B�B�fB��B��B��B��B��B�"B	�B	�B	)B	!�B	*B	'B	!�B	�B	HB	�B��B��B	�B	
�B	B	;B�B�"B	B	YB	AB	�B	6B	,qB	A�B	Y1B	q�B	wfB	�.B	��B	�HB	�zB	�wB	��B	�}B	�}B	�OB	�;B	�MB	��B	��B	�XB	��B	�B	�]B	��B	�3B	�B	̈́B	ΊB	�BB	�HB	��B	��B	̈́B	�vB	ԕB	�B	�yB	ۦB	�]B	��B	�B	�B	�B	�B	��B	�B	�B	�B	� B	��B	��B	�2B	�fB	�B	�B	�$B	�fB	�B	�hB	�BB	�B	ܒB	�B	��B	�B	��B	ܒB	��B	��B	��B	��B	�IB	�/B	��B	ݘB	��B	ܒB	�CB	��B	یB	�=B	�	B	�qB	یB	�qB	��B	��B	�/B	�B	ۦB	��B	��B	��B	�&B	ңB	ңB	�B	��B	�[B	��B	��B	�&B	��B	ԕB	�2B	�aB	��B	�{B	�FB	�,B	�{B	յB	՛B	�B	ּB	�
B	��B	�B	�sB	��B	�SB	�9B	�B	�B	�gB	յB	ԕB	�{B	ԯB	�
B	��B	�_B	��B	רB	خB	�B	�B	�,B	�9B	��B	ٚB	�+B	՛B	�2B	�$B	�mB	��B	�sB	��B	�yB	�_B	�+B	�_B	�yB	ؓB	ٚB	�B	�7B	�7B	�kB	�#B	�B	ܬB	�)B	�)B	ܬB	�B	��B	�B	�B	�5B	�jB	��B	��B	�bB	�hB	�B	�:B	�&B	�B	�&B	��B	�`B	�FB	��B	��B	�,B	�zB	�B	��B	�fB	�
B	�yB	�0B	�B	�B	�yB	�yB	�B	��B	�B	�B	��B	�B	��B	�)B	�IB	�oB	�B	�AB	�B	�vB	�vB	�[B	�B	�B	�GB	�|B	�B	��B	�MB	�B	�9B	�TB	��B	�tB	��B	��B	��B	�LB	��B	�8B	��B	��B	��B	��B	��B	�XB	��B	�DB	�DB	�^B	��B	�JB	��B	��B	��B	�}B	��B
  B
AB
uB
�B
�B
%B
YB
�B
�B
�B
9B
�B
�B
�B
�B
�B
�B
B
?B
�B
�B
�B

=B
�B
�B
1B
�B
�B
SB
SB
�B
�B
B
?B
�B
�B
	�B

�B
0B
�B
~B
B
B
dB
dB
�B
jB
�B
}B
�B
B
uB
�B
TB
TB
B
�B
2B
B
B
�B
B
aB
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B

B
?B
�B
sB
�B
�B
+B
_B
_B
�B
1B
QB
kB
B
�B
B
7B
�B
�B
	B
=B
=B
�B
�B
]B
�B
=B
�B
�B
�B
#B
�B
�B
#B
=B
	B
�B
�B
�B
�B
�B
�B
=B
CB
5B
jB
�B
!B
B
VB
�B
 �B
!B
!bB
!�B
!�B
"hB
#B
#B
"�B
"�B
"�B
#TB
$�B
%`B
%,B
%B
%�B
%�B
%�B
&fB
&�B
'8B
'B
'B
'�B
($B
(XB
(�B
)B
)_B
)�B
*KB
*�B
*�B
*�B
*�B
*�B
+6B
+6B
+6B
+B
+B
+�B
,"B
,�B
,�B
,qB
,qB
,WB
,"B
,qB
,B
+�B
+�B
+B
+kB
+6B
+QB
+QB
+�B
,=B
,=B
,�B
,�B
-B
,�B
-wB
-wB
-�B
-�B
./B
/5B
/B
/�B
/�B
0B
0�B
0�B
1[B
1[B
1[B
1�B
1�B
2aB
2aB
2aB
2|B
2|B
2|B
2aB
2|B
2�B
33B
3hB
4B
4�B
4�B
5tB
5�B
5�B
6B
6+B
6`B
6zB
6�B
6�B
7LB
7�B
88B
88B
88B
8�B
9>B
9>B
9�B
9�B
9�B
9�B
9�B
:�B
:^B
:^B
:�B
:�B
:�B
:�B
;B
;0B
;B
;B
;B
;�B
;�B
<B
<PB
<B
<B
<PB
<jB
<�B
="B
=�B
>BB
>wB
>�B
>�B
>�B
?}B
?}B
?�B
?�B
@iB
@�B
A;B
AUB
AoB
A�B
A�B
B'B
C-B
C�B
DB
DgB
D�B
EmB
ESB
E�B
E�B
FB
E�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
G�B
G�B
HB
HKB
H�B
H�B
I7B
IB
IB
IRB
IlB
J	B
J=B
J=B
J�B
KDB
K)B
KB
K�B
LB
L~B
LdB
LJB
L�B
L�B
MB
MB
MB
MPB
MPB
N"B
NVB
NVB
O(B
OvB
OvB
OvB
O�B
P.B
P}B
P�B
P�B
QB
Q�B
Q�B
R B
R�B
S@B
S@B
S[B
S�B
TB
T,B
TFB
TaB
T{B
T{B
T�B
T�B
T�B
T{B
T�B
T�B
UMB
U�B
U�B
VB
V�B
V�B
V�B
V�B
W?B
WYB
W�B
WsB
W�B
X+B
X_B
X�B
YeB
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZB
ZB
Z�B
Z�B
Z�B
[	B
[#B
[#B
[	B
[	B
[=B
[WB
[�B
[�B
[�B
\]B
\]B
\xB
\xB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]~B
^B
^B
^B
^5B
^B
^5B
^OB
^OB
^OB
^5B
^jB
^jB
^�B
^�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
`vB
`\B
`'B
`'B
`'B
`\B
`\B
`vB
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
bNB
bhB
b�B
cB
c�B
c�B
d&B
d@B
d@B
d&B
d&B
dtB
d�B
e,B
e`B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
gmB
g�B
g�B
h
B
h
B
hsB
h�B
h�B
h�B
iB
iB
iDB
iyB
i�B
i�B
jKB
jB
j�B
kB
kB
kB
kB
kQB
k�B
k�B
l"B
l=B
lWB
lqB
l�B
mCB
m�B
m�B
nB
nB
m�B
m�B
nB
oB
o B
oB
oOB
oOB
o�B
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
q'B
q'B
qB
q�B
rGB
raB
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
s�B
tB
tTB
tnB
t�B
uB
u�B
u�B
vB
v+B
v+B
v`B
v`B
v�B
v�B
wB
wLB
wLB
wfB
w�B
x8B
xlB
xRB
x8B
x�B
x�B
x�B
y$B
y>B
yXB
y�B
zB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
}B
}"B
}�B
}�B
}�B
}�B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�4B
��B
�B
� B
�;B
�;B
�;B
�UB
�oB
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�0B�0B��B��B��B�xB��B��B��B�xB��B��B��B��B��B��B��B��B��B�xB�^B�xB�^B�xB��B��B��B��B��B��B��B��B�B��B�zB�FB�+B��B��B��B�B�B��B�lB[BPbB	>]B	�$B
>�B
q�B
�tB
�?B
��B
h�B
1�B
5?B	�B	��B
-B
 �B	��B
B	��B	�4B	��B	�-B	�B	�7B	�XB	��B	�DB	�B	��B	�B	q'B	[=B	>�B	,WB	@B	B	B	YB	3B��B�QB�pB�B��B�B��B��B�DB��B	 B	^B	�B	jB	#nB	,B	(�B	$&B	�B	�B	�B��B�UB	�B	PB	fB	B��B�(B	�B	�B	-B	�B	�B	+�B	@�B	XyB	q�B	v�B	�.B	��B	��B	�2B	��B	��B	��B	��B	��B	�'B	�9B	��B	��B	��B	��B	�^B	��B	��B	ĶB	ɠB	�"B	��B	��B	� B	өB	уB	��B	�HB	�gB	�mB	ؓB	�B	��B	�OB	�vB	�B	��B	��B	�B	��B	�TB	�B	�nB	��B	�B	��B	��B	�>B	�_B	�B	�mB	�2B	��B	��B	�IB	��B	�CB	�)B	�]B	�xB	��B	�IB	�IB	ބB	ݲB	�B	��B	ޞB	�B	�B	�B	ܬB	�)B	��B	ۦB	یB	��B	�B	��B	�	B	��B	ݘB	��B	�B	�B	��B	��B	өB	�&B	�@B	��B	�FB	�uB	�&B	ӏB	ӏB	ӏB	�2B	՛B	��B	�2B	ԕB	�aB	�FB	ԯB	�B	��B	�2B	��B	�YB	ؓB	�_B	��B	�$B	֡B	֡B	ևB	֡B	յB	�B	ԯB	ԯB	�B	�YB	��B	ؓB	�yB	�EB	�B	��B	ּB	ԕB	ևB	�eB	�7B	��B	�9B	��B	��B	��B	�YB	רB	�B	خB	�yB	�yB	خB	خB	�B	�B	�kB	چB	�QB	ںB	�WB	�]B	��B	ܒB	ܒB	�B	ݘB	�5B	�OB	�OB	�jB	��B	��B	�B	��B	�B	��B	�nB	�ZB	�@B	�ZB	�B	�B	�zB	�B	��B	�zB	��B	��B	�2B	�B	�XB	�B	�eB	�KB	��B	��B	��B	��B	�0B	��B	�KB	�B	��B	�)B	�wB	�B	�B	��B	�B	�B	�B	��B	��B	�GB	�-B	�|B	�B	��B	�3B	��B	�9B	�TB	�B	�B	��B	�FB	�+B	��B	�fB	�2B	��B	��B	��B	��B	��B	�$B	��B	��B	�^B	�DB	�xB	��B	�dB	��B	�(B	��B	��B	��B
 4B
[B
uB
�B
�B
?B
tB
zB
�B
�B
SB
B
B
�B
�B
gB
�B
�B
�B
�B
0B
B

XB
	B
�B
KB
1B
�B
mB
SB
EB
�B
YB
�B
�B
�B
	�B
B
JB
�B
�B
�B
jB
�B
~B
�B
�B
�B
�B
�B
@B
�B
B
�B
TB
B
�B
MB
B
SB
B
�B
�B
FB
aB
gB
�B
B
�B
B
B
,B
�B
2B
B
�B
B
mB
�B

B
YB
�B
�B
�B
B
+B
_B
�B
�B
KB
kB
�B
�B
B
7B
7B
�B
�B
#B
=B
WB
�B
CB
�B
B
WB
�B
�B
�B
qB
B
�B
qB
�B
=B
�B
�B
�B
	B
	B
	B
qB
�B
OB
�B
B
;B
!B
pB
�B
!B
!HB
!�B
!�B
!�B
"hB
#B
#B
"�B
"�B
"�B
#�B
$�B
%�B
%`B
%,B
%�B
%�B
%�B
&�B
'B
'8B
'B
'mB
'�B
($B
(sB
(�B
)B
)_B
)�B
*KB
*�B
*�B
*�B
*�B
*�B
+6B
+6B
+QB
+QB
+kB
+�B
,qB
,�B
,�B
,�B
,�B
,qB
,WB
,�B
,"B
,"B
+�B
+6B
+kB
+QB
+�B
+�B
,B
,�B
,qB
,�B
,�B
-)B
-B
-wB
-wB
-�B
.B
.}B
/5B
/OB
/�B
/�B
0!B
0�B
0�B
1vB
1[B
1vB
1�B
1�B
2aB
2aB
2aB
2aB
2|B
2|B
2|B
2�B
3B
3MB
3�B
4B
4�B
5%B
5�B
5�B
5�B
6+B
6+B
6`B
6�B
6�B
72B
7fB
8B
88B
88B
8RB
8�B
9XB
9XB
9�B
9�B
9�B
9�B
:*B
:�B
:^B
:xB
:�B
:�B
;B
:�B
;B
;JB
;B
;JB
;�B
;�B
<B
<PB
<PB
<6B
<6B
<jB
<�B
<�B
=<B
=�B
>BB
>wB
>�B
>�B
?.B
?�B
?�B
?�B
@B
@�B
AB
A;B
AUB
A�B
A�B
A�B
B'B
C-B
C�B
D3B
D�B
EB
E�B
EmB
E�B
E�B
F%B
E�B
E�B
E�B
E�B
F%B
F�B
F�B
F�B
GB
GzB
G�B
G�B
G�B
G�B
G�B
G�B
H1B
HKB
H�B
IB
IRB
IB
IB
IlB
I�B
J#B
JrB
JXB
J�B
KDB
KDB
K)B
K�B
LB
L~B
LdB
LJB
L�B
L�B
MB
MB
MB
MjB
MjB
NVB
NVB
N�B
OBB
OvB
OvB
O�B
O�B
PHB
P}B
P�B
Q B
Q4B
RB
RB
RTB
SB
S[B
S@B
SuB
S�B
T,B
TFB
TFB
TaB
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UgB
U�B
U�B
V9B
V�B
V�B
W
B
V�B
WYB
WsB
W�B
WsB
W�B
X+B
XyB
X�B
YB
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[#B
[=B
[WB
[�B
[�B
[�B
\B
\]B
\xB
\�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
\�B
\�B
\�B
]/B
]�B
^5B
^B
^B
^5B
^B
^OB
^jB
^jB
^OB
^OB
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
`vB
`vB
`'B
`BB
`BB
`vB
`\B
`vB
`�B
`�B
`�B
a-B
a�B
a�B
a�B
bB
bNB
b�B
b�B
c B
c�B
dB
d&B
d@B
d@B
d&B
d@B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
gRB
gmB
g�B
g�B
h$B
h
B
h�B
h�B
h�B
h�B
i*B
iB
iDB
i�B
i�B
jB
jeB
j�B
j�B
kB
kB
k6B
kB
k�B
k�B
k�B
l=B
lWB
lWB
lqB
l�B
mCB
m�B
m�B
nB
n/B
m�B
m�B
n/B
oB
o B
oB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
qAB
q'B
q'B
q�B
raB
r|B
r�B
r�B
sB
s3B
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
u%B
u�B
u�B
vB
vFB
v+B
v`B
v`B
v�B
v�B
w2B
wfB
wfB
w�B
w�B
x8B
xlB
xRB
xRB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
zB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{B
{0B
{dB
{dB
{�B
{�B
|B
|6B
|PB
|�B
|�B
|�B
}"B
}<B
}�B
}�B
}�B
~B
~�B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�4B
��B
�B
�B
�;B
�;B
�;B
�oB
��B
��B
��B
��B
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
<SZ�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�<#�
<#�
<5ު<#�
<#�
<2��<7�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.12(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002190057132020021900571320200219005713202207271134412022072711344120220727113441202207271537042022072715370420220727153704  JA  ARFMdecpA30a                                                                20200207183738  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200207183928  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200207183929  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200207183930  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200207183930  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200207183930  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200207183930                      G�O�G�O�G�O�                JA  ARUP                                                                        20200207185506                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200209000000  CF  PSAL_ADJUSTED_QC@=p�@=p�G�O�                JM  ARCAJMQC2.0                                                                 20200218155713  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200218155713  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023441  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063704  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                