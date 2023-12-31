CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-10T09:38:23Z creation;2019-10-10T09:38:25Z conversion to V3.1;2022-08-02T05:11:59Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191010093823  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_025                    2C  D   APEX                            8420                            2.11.2                          846 @���d�
 1   @������ @/�=�b��c��j~��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B���B�33B���B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�33B�33B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��
@�33AG�A ��AAp�Aa�A���A��\A���A��RA�z�A�z�A�ffA�=qB 33B33B�BQ�B 33B(
=B033B8(�B@=qBH=qBPG�BX=qB`Q�Bh\)Bp=qBx(�B�#�B�33B�B�B�u�B��3B�\B�\B�B�{B�33B�\B�W
B�.B�  B�G�B�B��B�
=B��B�\B�.B��fB��fB�B�{B�{B�#�B�B�B�ffB�.B��B�C C
=C\C�C�C
�C
C{C\C�C\CCC�C�C!HC +�C"�C$\C&
=C(�C*\C,�C.
=C0\C2�C4{C6�C8
=C:�C<C>�C@�CB�CD
=CFCHCJ�CL\CN\CP\CR
=CT�CV)CX
CZ�C\�C^
=C`
=Cb�Cd�Cf�Ch�Cj
=Cl�Cn\Cp�Cr�Ct�Cv\Cx�Cz�C|\C~\C��C�
=C��C��C��C��C�fC�fC�C�fC��C��C�
=C��C��C�fC�fC�C��C�
=C��C��C��C�C�fC�fC��C��C�
=C��C�fC�fC��C��C��C��C�fC��C�
=C�
=C�fC��C�fC��C��C��C�fC��C�C��C��C�C��C��C�fC�fC��C�C��C�fC��C��C�C�C�fC�fC�C�fC��C�
=C��C�C�fC�C��C��C��C�C�C��C��C�fC�fC�C�fC��C��C��C�fC�fC��C�HC��C��C�fC��C�fC��C��C��C�
=C��C��C��C�C��C�
=C�C�fC��C�C��C��C�C��C�C�
=C��C��C��C��C�C��C�fC��C�C�C��D {D �DD�3D�D�fD�D��D{D�3D3D�D{D��DD��D3D��D	�D	�fD
{D
�3D�D��DD�D�D�{D3D��D�D��D�D�3D�D��D�D��D3D��D{D��DD��D�D��D�D�{D�D��D�D�{D�D�{D{D�D�D��D{D�{D{D�D{D�D {D �3D! �D!��D"D"�D#�D#��D$�D$�{D%�D%��D&3D&�D'{D'�HD(3D(�
D)RD)�fD*3D*��D+3D+�3D,{D,�{D-3D-�3D.3D.�3D/{D/��D0HD0��D1D1�{D2�D2��D3�D3��D4{D4�D53D5��D63D6�{D7{D7�{D8{D8��D9�D9��D:�D:�D;�D;��D<{D<�3D=3D=�{D>3D>��D?{D?�{D@3D@��DAfDA�DB�DB��DCHDC�3DDDD�{DE�DE�3DF{DF�DG�DG�{DH3DH��DI�DI��DJfDJ��DKDK��DL{DL��DM�DM�fDN�DN��DO�DO��DP�DP�3DQ{DQ��DR3DR�3DS�DS�3DT3DT��DU�DU�3DV�DV��DW�DW�DX3DX�3DY�DY��DZ�DZ�3D[3D[��D\�D\��D]HD]��D^{D^�3D_�D_�D`{D`��Da�Da�3Db{Db��Dc �Dc��Dd�Dd��DeDe�DfDf�{Dg3Dg��Dh�Dh�{Di3Di��Dj �Dj��Dk{Dk��Dl�Dl�DmDm��Dn
Dn�Do�Do�3Dp�Dp�3Dq{Dq��Dr{Dr��DsDs�{Dt�Dt��Du�Du��Dv�Dv��Dw3Dw�Dx�Dx�{DyDy��Dz
Dz�{D{�D{�3D|�D|��D}{D}��D~D~�{D�D��D��D�B�D���D���D�HD�A�D���D��=D��D�B�D��=D��=D��D�A�D���D���D��D�B�D���D���D��D�A�D���D�D�=D�B�D���D���D��D�A�D���D���D��D�A�D���D���D��D�B�D��=D��=D��D�B�D���D���D��D�AHD��=D��=D��D�A�D���D���D� �D�A�D���D��=D��D�B=D���D��HD� �D�@�D���D��=D��D�A�D���D��HD� �D�B=D���D��=D� �D�@�D��HD���D�3D�B�D��=D���D�=D�B=D���D��=D�=D�B=D���D���D��D�A�D���D��HD��D�B=D��=D���D��D�B=D���D���D��D�B=D���D���D� �D�AHD���D���D�=D�A�D���D���D��D�B�D��=D���D��D�B=D���D��=D�HD�A�D���D�D�=D�B=D��HD��RD��D�A�D��HD��=D�=D�@�D���D��=D�=D�A�D��HD���D��D�A�D��3D�D��D�B=D���D���D��D�B�D���D���D�HD�@�D���D��3D��D�B�D���D�D�=D�B=D��3D�ÅD��D�B=D���D��=D��D�A�D���D���D�HD�B=D��HD���D��D�B=D���D���D��D�B=D���D���D�HD�A�D��=D���D�=D�@�D��HD���D��D�A�D���D��HD��D�A�D���D���D�=D�B=D���D���D�=D�A�D���D���D��D�B=D���D��=D��D�A�D���D��=D��D�B=D��=D�D��D�A�D��HD���D��D�B=D���D��=D�=D�AHD��HD���D��D�B=D���D���D�=D�B�D��3D���D� �D�A�D��=D���D��D�A�D��HD��RD��D�B=D��HD�D��D�B�D���D��=D��D�@�D���D���D� �D�@�D���D�D��D�@�D=D���D��D�B=DÁ�D���D��D�A�DĀRD���D��D�B�Dł=D��HD� �D�@�DƁHD���D�=D�B�Dǁ�D��HD�=D�C�DȂ�D�D�3D�B=Dɂ�D���D�=D�A�Dʂ=D���D�=D�B�Dˁ�D�D��D�A�D̂=D���D� �D�A�D́�D��=D��D�B=D΂�D���D�HD�AHDρHD���D��D�B=DЂ=D�D�=D�A�Dт=D���D� �D�AHD҂�D���D��D�A�DӁHD���D��D�B=Dԁ�D���D��D�@�DՁ�D�D�=D�B=Dւ�D���D�HD�A�Dׁ�D���D��D�A�D؁HD��HD��D�B=Dـ�D���D�HD�@�Dځ�D�D��D�C3Dہ�D���D��D�C3D܃�D���D��D�A�D݁�D��=D�HD�A�Dށ�D���D� �D�A�D߂�D��=D�HD�AHD���D���D�=D�B�D�=D���D� �D�AHD�HD���D�=D�B=D��D��=D��D�AHD�HD���D�=D�A�D�=D�D��D�B=D恚D���D�=D�B=D��D���D��D�B�D�3D���D��D�C�D郅D��=D�HD�A�D�HD��HD��D�B�D�=D���D� �D�B�D��D��=D��D�A�D��D���D��D�A�D�HD��=D��D�@�D��D�D��D�@�D���D���D��D�A�D��D���D��D�C3D�=D��HD��D�B�D�3D���D��D�B=D�D���D� �D�@RD��=D���D� RD�@�D���D���D�=D�C3D���D���D��D�B�D���D���D��D�A�D���D��=D��D�A�D���D���D��D�A�D�c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�"A�VA��A�A��A��A��A�(A�\A�.A� A�A��A��A� A��A�VA��A�.A��A��A�:A�{A�\A�\A�bA�4A��A��A��A��A��A��A��AۂuA�ZAյtA� 'AљeA�?}A�e`Aə�A�3�Aơ�A��#A�A�A���A���A���A��=A�!A�e�A��A��A��A��A�v+A�T�A��NA���A���A��A�L0A���A��DA�3�A��AA��A�~�A�GEA�MA�WsA|��Ay<�Aw��Ap�Aj�hAe֡Ac�Aa@OA^�AZRTAX7LAU�&AS�~ARG�AQ��AP&�ANxAK�<AI�>AG��AC�A?��A<�vA;�?A9�XA8A7Q�A5�!A3	A1�"A.�A-��A)�+A&�hA&�A$6A#9XA"��A"ffA"�A!!A �FA �4A ��A �YA OvA �A� AsA&�A;A�A��A�PA�KA@OA�-Ay�AP�A�`A�ZA�`A�*A6A�QAPHA��A�An/A�A~�AkQAB[A��Av`A4AJA��A��A��A[�A�PA�aAFtA�A��A�HA�fAn�A�AE�A
=AR�A��A�'A?�A��Aa|AOvAߤA��A3�A
��A
"�A	�4A�Ax�AVmA��A�YA�AԕA\)A/�A��A~�A"�A��A�Au�AYA�1A��A6zA ߤA 5�@��@��V@�+@���@�qv@��'@�2�@��@���@�R�@�<�@���@��@�l�@���@���@�iD@���@�6z@��@�D@��z@�9�@��s@�x@�=q@���@�O@��D@�+k@��@��@��@�+�@���@��@���@��@��)@��@�/�@��@��@�l"@���@�@�b�@�z@��@��@�o @�T�@�K�@�)_@�@���@�m�@�{@���@��@��p@�;�@��)@�|�@���@�9�@��@��B@�~�@��]@۾w@ۃ{@��@ګ6@�I�@ٴ�@�/@،�@��.@�e�@�L0@��r@��@՜@�;d@���@�GE@��A@�{J@�e�@�m]@���@�!�@��#@�o�@�q�@��W@�b�@��@�u%@��z@�J�@�@��)@�z@�YK@�@�@�ƨ@�Mj@�q@��K@�p;@��A@Ɍ~@�@�h�@�7�@�O@ǲ�@�s�@�S&@�+�@��@Ƥ�@�u%@�{@��
@Ő�@Ľ<@��&@Ç�@�$t@��@ �@�oi@�-@���@�5�@��O@�bN@�~�@��H@���@�b@���@���@���@���@��Q@�e�@�K�@�9�@��P@��@�=@���@��r@�3�@��m@�.I@��r@�|�@�v�@�oi@�1'@��#@�m]@��@��1@�!@���@��@���@�1�@�ی@���@�V�@�-@�M@��>@�ϫ@���@�rG@�(�@��@���@�?@���@��M@��@��R@���@�K^@���@�[W@��`@���@�(�@��W@��@�'�@���@��u@�a|@�@��T@���@�U�@��@���@�S�@��@���@��@���@�7�@��-@�p�@�+�@��@�h�@�b@��@��@���@��@�6@��@��K@���@��X@�.I@�ȴ@���@���@�j@�,=@�M@��@��w@�t�@�@��I@�Z@�$�@��@��@���@�m]@���@��4@�xl@�7@��[@�c@�f�@�f�@�X�@�+�@��@��@���@��@�J�@���@��X@�"�@�~�@�E�@�@��&@��X@��{@�'�@��$@�V�@�($@��@��W@��"@�Q�@���@��[@��e@���@���@�h
@�<�@��m@�t�@�Dg@�:�@�0�@��@��)@���@�q�@�PH@�x@��[@��4@�K�@�=@�/�@��@���@���@���@�Z�@�&�@��6@��k@��7@�`B@�2a@���@���@�!�@���@���@�Z�@�%F@�S@���@��!@���@�Ta@�A�@�)�@��m@��@@�e�@�1�@��c@��?@���@�V�@�1'@���@�y�@��@��@��R@���@�xl@�V@�;�@�)�@�J@��@���@��:@�X@�8@��@���@�֡@���@��\@�`�@�+k@��H@���@�_p@�V@���@��@��@��+@���@�v�@�C-@���@�ϫ@���@���@�Z�@�=�@��@���@�Q@� �@��
@���@���@���@�w2@�Y�@�@@��H@���@��@�GE@�$�@��@�@��@dZ@~��@~3�@}��@|w�@{�@{�@{y�@{&@z$�@y��@x�P@xr�@x@w�:@v��@v��@vR�@v
�@u[W@tl"@s��@s��@s�f@s~�@s4�@r�@r�A@q�@qo @q=�@p�/@pq@o��@o�&@oqv@n��@n}V@n_�@nGE@n=q@n5?@n@m�>@m/@lѷ@l��@lG@k]�@j��@j�1@jff@j($@ix�@i^�@iA @i	l@h��@g�@f��@f�\@f@e��@e0�@d�@d��@dN�@c��@c;d@c
=@b��@bߤ@b��@bM�@a��@as�@`ѷ@`��@`�@`N�@`b@_��@_�0@_��@_6z@^��@^�,@^��@^-@]�j@]�'@]hs@]q@\��@\�@[��@[��@[4�@Z�"@Zxl@Zi�@Zff@ZGE@Z�@Y�o@Y�@Y�'@YA @XĜ@W��@W)_@V�@U�@UG�@T�@T�@S��@SdZ@So@R�,@Rxl@R.�@Q�o@Q�t@QT�@Q@Pی@P1'@O�F@O{J@OC�@N��@N4@M�@M��@MJ�@L�@L��@L2�@K��@K�@K�q@K�@J��@JV@I�>@I�X@IN<@H�@Hm�@H�@G�a@Gl�@G"�@F��@F�@E�@E��@EDg@E�@DFt@C�&@CiD@CK�@C'�@B�@B��@ArG@@��@@>B@?�*@?l�@?�@>��@>�X@>�+@>3�@=��@=m]@=�@<�@<��@< �@;�r@;��@;O@:�<@:z@:#:@9��@9\�@9IR@9<6@9@8�z@8Xy@8*�@7�A@7��@6��@6��@6�1@6v�@6=q@5��@5a�@4��@4�@4�?@4��@4e�@3�@3��@3��@3O@34�@3�@2ں@2�@2�A@2e@1�#@1��@1��@1`B@0Ĝ@0<�@/�*@/E9@/C@.�X@.z@.#:@-�D@-�@-�~@-m]@-<6@-�@,�P@,�`@,�j@,l"@, �@+�6@+��@+U�@+'�@*�c@*kQ@*B[@*e@)�>@)�z@)�@)��@)�@)c�@(�)@(@'�+@'�K@'��@'�@'��@'�$@'t�@'\)@'�@&��@&�+@&\�@&H�@%�D@%��@%J�@%F@%7L@%@$��@$�$@$oi@$U2@$4n@#��@#�@#_p@#Y@"ں@"~�@"H�@"�@!��@!T�@!@ ��@ �o@ r�@ @g�@33@'�@&@"�@(@��@GE@��@�X@u�@Y�@/@�@@@��@�`@Ɇ@�_@`�@1'@�@�@ƨ@�P@dZ@&@�H@��@Z�@?@	@��@��@L�@#�@�@��@Ɇ@�j@�O@��@�u@c�@9X@7@��@�6@�@~�@a@9�@'�@C@�@�@�R@v�@5?@)�@O@�>@�M@Q�@*0@%@�@�|@�@�/@�?@��@�@I�@�@�@�@�[@{J@RT@"�@�y@�!@��@��@c @W�@?@3�@_@�@�C@�S@rG@G�@�@%@�@�@�[@��@�z@|�@V�@K^@C-@1'@G@ݘ@�6@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�"A�VA��A�A��A��A��A�(A�\A�.A� A�A��A��A� A��A�VA��A�.A��A��A�:A�{A�\A�\A�bA�4A��A��A��A��A��A��A��AۂuA�ZAյtA� 'AљeA�?}A�e`Aə�A�3�Aơ�A��#A�A�A���A���A���A��=A�!A�e�A��A��A��A��A�v+A�T�A��NA���A���A��A�L0A���A��DA�3�A��AA��A�~�A�GEA�MA�WsA|��Ay<�Aw��Ap�Aj�hAe֡Ac�Aa@OA^�AZRTAX7LAU�&AS�~ARG�AQ��AP&�ANxAK�<AI�>AG��AC�A?��A<�vA;�?A9�XA8A7Q�A5�!A3	A1�"A.�A-��A)�+A&�hA&�A$6A#9XA"��A"ffA"�A!!A �FA �4A ��A �YA OvA �A� AsA&�A;A�A��A�PA�KA@OA�-Ay�AP�A�`A�ZA�`A�*A6A�QAPHA��A�An/A�A~�AkQAB[A��Av`A4AJA��A��A��A[�A�PA�aAFtA�A��A�HA�fAn�A�AE�A
=AR�A��A�'A?�A��Aa|AOvAߤA��A3�A
��A
"�A	�4A�Ax�AVmA��A�YA�AԕA\)A/�A��A~�A"�A��A�Au�AYA�1A��A6zA ߤA 5�@��@��V@�+@���@�qv@��'@�2�@��@���@�R�@�<�@���@��@�l�@���@���@�iD@���@�6z@��@�D@��z@�9�@��s@�x@�=q@���@�O@��D@�+k@��@��@��@�+�@���@��@���@��@��)@��@�/�@��@��@�l"@���@�@�b�@�z@��@��@�o @�T�@�K�@�)_@�@���@�m�@�{@���@��@��p@�;�@��)@�|�@���@�9�@��@��B@�~�@��]@۾w@ۃ{@��@ګ6@�I�@ٴ�@�/@،�@��.@�e�@�L0@��r@��@՜@�;d@���@�GE@��A@�{J@�e�@�m]@���@�!�@��#@�o�@�q�@��W@�b�@��@�u%@��z@�J�@�@��)@�z@�YK@�@�@�ƨ@�Mj@�q@��K@�p;@��A@Ɍ~@�@�h�@�7�@�O@ǲ�@�s�@�S&@�+�@��@Ƥ�@�u%@�{@��
@Ő�@Ľ<@��&@Ç�@�$t@��@ �@�oi@�-@���@�5�@��O@�bN@�~�@��H@���@�b@���@���@���@���@��Q@�e�@�K�@�9�@��P@��@�=@���@��r@�3�@��m@�.I@��r@�|�@�v�@�oi@�1'@��#@�m]@��@��1@�!@���@��@���@�1�@�ی@���@�V�@�-@�M@��>@�ϫ@���@�rG@�(�@��@���@�?@���@��M@��@��R@���@�K^@���@�[W@��`@���@�(�@��W@��@�'�@���@��u@�a|@�@��T@���@�U�@��@���@�S�@��@���@��@���@�7�@��-@�p�@�+�@��@�h�@�b@��@��@���@��@�6@��@��K@���@��X@�.I@�ȴ@���@���@�j@�,=@�M@��@��w@�t�@�@��I@�Z@�$�@��@��@���@�m]@���@��4@�xl@�7@��[@�c@�f�@�f�@�X�@�+�@��@��@���@��@�J�@���@��X@�"�@�~�@�E�@�@��&@��X@��{@�'�@��$@�V�@�($@��@��W@��"@�Q�@���@��[@��e@���@���@�h
@�<�@��m@�t�@�Dg@�:�@�0�@��@��)@���@�q�@�PH@�x@��[@��4@�K�@�=@�/�@��@���@���@���@�Z�@�&�@��6@��k@��7@�`B@�2a@���@���@�!�@���@���@�Z�@�%F@�S@���@��!@���@�Ta@�A�@�)�@��m@��@@�e�@�1�@��c@��?@���@�V�@�1'@���@�y�@��@��@��R@���@�xl@�V@�;�@�)�@�J@��@���@��:@�X@�8@��@���@�֡@���@��\@�`�@�+k@��H@���@�_p@�V@���@��@��@��+@���@�v�@�C-@���@�ϫ@���@���@�Z�@�=�@��@���@�Q@� �@��
@���@���@���@�w2@�Y�@�@@��H@���@��@�GE@�$�@��@�@��@dZ@~��@~3�@}��@|w�@{�@{�@{y�@{&@z$�@y��@x�P@xr�@x@w�:@v��@v��@vR�@v
�@u[W@tl"@s��@s��@s�f@s~�@s4�@r�@r�A@q�@qo @q=�@p�/@pq@o��@o�&@oqv@n��@n}V@n_�@nGE@n=q@n5?@n@m�>@m/@lѷ@l��@lG@k]�@j��@j�1@jff@j($@ix�@i^�@iA @i	l@h��@g�@f��@f�\@f@e��@e0�@d�@d��@dN�@c��@c;d@c
=@b��@bߤ@b��@bM�@a��@as�@`ѷ@`��@`�@`N�@`b@_��@_�0@_��@_6z@^��@^�,@^��@^-@]�j@]�'@]hs@]q@\��@\�@[��@[��@[4�@Z�"@Zxl@Zi�@Zff@ZGE@Z�@Y�o@Y�@Y�'@YA @XĜ@W��@W)_@V�@U�@UG�@T�@T�@S��@SdZ@So@R�,@Rxl@R.�@Q�o@Q�t@QT�@Q@Pی@P1'@O�F@O{J@OC�@N��@N4@M�@M��@MJ�@L�@L��@L2�@K��@K�@K�q@K�@J��@JV@I�>@I�X@IN<@H�@Hm�@H�@G�a@Gl�@G"�@F��@F�@E�@E��@EDg@E�@DFt@C�&@CiD@CK�@C'�@B�@B��@ArG@@��@@>B@?�*@?l�@?�@>��@>�X@>�+@>3�@=��@=m]@=�@<�@<��@< �@;�r@;��@;O@:�<@:z@:#:@9��@9\�@9IR@9<6@9@8�z@8Xy@8*�@7�A@7��@6��@6��@6�1@6v�@6=q@5��@5a�@4��@4�@4�?@4��@4e�@3�@3��@3��@3O@34�@3�@2ں@2�@2�A@2e@1�#@1��@1��@1`B@0Ĝ@0<�@/�*@/E9@/C@.�X@.z@.#:@-�D@-�@-�~@-m]@-<6@-�@,�P@,�`@,�j@,l"@, �@+�6@+��@+U�@+'�@*�c@*kQ@*B[@*e@)�>@)�z@)�@)��@)�@)c�@(�)@(@'�+@'�K@'��@'�@'��@'�$@'t�@'\)@'�@&��@&�+@&\�@&H�@%�D@%��@%J�@%F@%7L@%@$��@$�$@$oi@$U2@$4n@#��@#�@#_p@#Y@"ں@"~�@"H�@"�@!��@!T�@!@ ��@ �o@ r�@ @g�@33@'�@&@"�@(@��@GE@��@�X@u�@Y�@/@�@@@��@�`@Ɇ@�_@`�@1'@�@�@ƨ@�P@dZ@&@�H@��@Z�@?@	@��@��@L�@#�@�@��@Ɇ@�j@�O@��@�u@c�@9X@7@��@�6@�@~�@a@9�@'�@C@�@�@�R@v�@5?@)�@O@�>@�M@Q�@*0@%@�@�|@�@�/@�?@��@�@I�@�@�@�@�[@{J@RT@"�@�y@�!@��@��@c @W�@?@3�@_@�@�C@�S@rG@G�@�@%@�@�@�[@��@�z@|�@V�@K^@C-@1'@G@ݘ@�6@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	�B	�B	�;B	�UB	�oB	�oB	�;B	�UB	�oB	�UB	�UB	�;B	�!B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�iB	�5B	��B	~�B	i�B	Y1B	[�B	]�B	d&B	m�B	rGB	s�B	}VB	�B	��B	�kB	�~B	��B	�DB	��B	��B	�SB	ޞB	�B	�qB	��B	��B
�B
H1B
��B
�hB
o�B
F�B
,WB
�B
�B
�B
<B	�MB	��B	�7B	��B	��B	�B	l"B	H�B	1�B	&�B	KB	6B	B��B�5B��B��B�BB�kB�&B��B�B��B��B�eB��B�B�AB�lB�DB�0B��B��B�B�]B��B��B�$BȀBרB�-B�B��B�/B��B	�B	�B	�B	�B	�B	'B	*B	8B	=�B	`BB	c B	iyB	m)B	xRB	.B	�SB	��B	�B	��B	��B	�mB	��B	�>B	�>B	�KB	�qB	�]B	��B	��B	�B	��B	�'B	��B	�GB	�B	��B	�[B	��B	�B	�-B	�MB	��B	�zB	�B	��B	��B	��B	�VB	��B	�iB	��B	� B	� B	��B	��B	��B	�oB	��B	��B	��B	�;B	�UB	�[B	��B	�3B	��B	ĶB	�mB	��B	�B	�	B	�#B	�0B	��B	�B	��B	͹B	��B	ΊB	��B	� B	�4B	�B	�[B	�B	�&B	�@B	��B	��B	�B	�&B	ҽB	��B	� B	ѷB	�hB	�}B	ΥB	��B	� B	�NB	��B	�vB	�B	ϑB	уB	�oB	� B	�TB	҉B	ҽB	ѷB	ЗB	οB	�VB	��B	ʌB	��B	�fB	ʌB	�RB	�tB	�SB	�9B	ɆB	��B	�B	�KB	�+B	�zB	ƎB	ȚB	�^B	�B	�B	�.B	�B	�bB	��B	ӏB	�{B	ԕB	�?B	�EB	��B	�kB	�]B	��B	ݲB	��B	�pB	�bB	�4B	�B	� B	�@B	��B	�B	�FB	��B	�2B	�B	�mB	�B	��B	�B	��B	�zB	��B	��B	�B	�ZB	�&B	��B	�
B	�B	�B	�B	�B	�RB	��B	�XB	�B	�mB	�RB	�B	�B	�B	��B	�B	��B	�0B	�B	�QB	�B	��B	�=B	��B	��B	��B	�B	�B	��B	�B	�qB	�B	�)B	�B	�)B	�]B	�B	��B	� B	�iB	�B	�B	�'B	�B	��B	�|B	�3B	�B	�%B	��B	�+B	��B	��B	�B	�$B	��B	�rB	��B	�$B	�rB	��B	��B	��B	�B	��B	�6B	��B	��B	��B	�(B	��B	�wB	�wB	�wB	��B	��B	��B	�B	��B	�}B	��B
 B
�B
B
AB
B
�B
�B
�B
�B
�B
�B
�B
�B
B
gB
B
�B
�B
%B
tB
YB
�B
�B
zB
EB
zB
KB
fB
	7B

=B

�B

XB

�B
�B
�B
0B
�B
�B
6B
�B
�B
<B
BB
BB
�B
�B
bB
�B
�B
B
:B
�B
B
@B
uB
�B
,B
aB
aB
,B
�B
B
2B
B
MB
gB
gB
gB
�B
B
�B
�B
$B
�B
�B
�B
YB
?B
B
�B
+B
�B
KB
B
eB
KB
KB
KB
KB
eB
B
B
KB
�B
�B
�B
B
�B
B
�B
#B
#B
#B
�B
�B
�B
�B
)B
�B
!B
�B
�B
�B
VB
!B
VB
 �B
 'B
!�B
"B
"B
"4B
"�B
"�B
#B
#B
#B
#:B
#nB
#�B
#�B
#�B
#�B
$&B
$�B
$�B
$�B
%B
%FB
%zB
%�B
%zB
%zB
&LB
&fB
%�B
%�B
&B
&�B
&�B
'B
'RB
'�B
'�B
'�B
(>B
(>B
(XB
(�B
(�B
)DB
)yB
)�B
)�B
*KB
*0B
*B
*�B
+QB
+�B
+�B
+�B
+�B
,B
+�B
,=B
,=B
,�B
-)B
-wB
-�B
-CB
-�B
.B
.�B
/�B
0;B
0�B
0�B
1'B
1�B
1AB
1�B
2B
2-B
2|B
3�B
3�B
3�B
3�B
3�B
4B
4B
49B
4�B
4nB
49B
4B
4TB
49B
4�B
4�B
5B
5%B
5?B
5?B
5ZB
5�B
5�B
5�B
5�B
6zB
6zB
6zB
6�B
7LB
7�B
7�B
7�B
7fB
8B
8lB
8�B
8�B
8lB
88B
8�B
8�B
9>B
9�B
9$B
9$B
9XB
9�B
9�B
9XB
9$B
9$B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:�B
;�B
<B
;B
;JB
;B
;B
;dB
;dB
;dB
;dB
;JB
;dB
;JB
;�B
;�B
;�B
<6B
<jB
<�B
="B
=qB
=�B
=�B
=�B
=�B
=�B
>B
?B
?}B
?�B
?�B
@OB
@�B
@�B
AoB
A�B
B'B
BB
BAB
BAB
BAB
B[B
B�B
CGB
C{B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
D�B
EB
EB
EB
EmB
E�B
E�B
E�B
F%B
F?B
F�B
F�B
F�B
F�B
G+B
GEB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H1B
H�B
I7B
I�B
I�B
JXB
JrB
J�B
J�B
K�B
K�B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MB
MB
M�B
M�B
M�B
NB
NpB
N�B
N�B
O(B
OB
O(B
OvB
O�B
OvB
OvB
O�B
O�B
P.B
P�B
P�B
P�B
QB
QNB
QNB
QhB
Q�B
Q�B
R�B
SB
S@B
SuB
S�B
SuB
T,B
TFB
T�B
T�B
T�B
T�B
UB
VB
U�B
V9B
V�B
V�B
W
B
W
B
V�B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
XyB
XyB
X�B
YB
YeB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\)B
]B
]/B
]IB
]/B
]B
]B
]B
]/B
\�B
]B
]IB
]dB
]�B
^B
^B
^B
^jB
_B
_!B
_!B
_!B
_pB
_�B
_�B
`'B
`'B
`�B
`�B
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
b�B
cB
cB
c B
c B
c:B
cnB
c�B
dB
d&B
dtB
d�B
e,B
eFB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
gB
f�B
gB
g8B
g8B
g8B
gRB
gRB
gRB
g�B
h
B
h$B
h>B
h$B
h�B
h�B
iB
h�B
h�B
iB
h�B
i*B
i_B
iyB
iyB
i�B
i�B
j0B
jeB
jB
j�B
kB
kB
k�B
k�B
lB
lqB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
o B
o5B
oiB
oiB
oiB
o�B
o�B
o�B
o�B
p!B
p;B
pUB
poB
p�B
qB
q'B
qvB
q�B
q�B
rGB
raB
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tTB
tnB
tnB
t�B
t�B
uB
u?B
uZB
uZB
uZB
utB
u�B
u�B
v+B
u�B
u�B
vB
v�B
v�B
v�B
w2B
wLB
w2B
wLB
wLB
wfB
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
y	B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
zB
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{dB
{B
{�B
{�B
{�B
{�B
{�B
|B
|B
|jB
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�B	��B	�B	�B	�;B	�UB	�oB	�oB	�;B	�UB	�oB	�UB	�oB	�UB	�!B	��B	�B	��B	�B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�UB	�GB	�9B	�B	o B	^OB	_�B	cTB	j�B	q'B	vFB	|�B	�EB	�B	�B	�pB	�HB	��B	�B	��B	�3B	�B	��B	�[B	�B	�B	�*B
GB
J=B
�oB
�?B
u�B
K^B
/�B
�B
�B
�B
&B	��B	өB	�B	�B	��B	��B	sB	M�B	4�B	*eB	�B	hB	�B��B��B�_B�@B�hB��BևB��BĜB��B�FB��B�sB��B�3B��B��B�BB��B�qB�&B��B�+B�lB�dBɺB�_B�B�B�&B��B��B	�B		7B	�B	�B	OB	'�B	+�B	8�B	=B	`vB	c:B	iyB	m�B	x�B	�B	��B	�EB	�"B	��B	��B	�
B	�>B	�B	�DB	�B	�)B	�B	�IB	��B	��B	�;B	��B	�-B	��B	��B	�|B	��B	�AB	��B	��B	��B	��B	��B	�lB	�B	��B	�B	�(B	�iB	�;B	�UB	��B	��B	�AB	B	��B	�B	�oB	�oB	��B	��B	�'B	��B	ÖB	āB	ĜB	ňB	�tB	�B	ɠB	�XB	��B	�dB	�JB	̈́B	�"B	�"B	�pB	�\B	��B	ѷB	��B	��B	ӏB	�[B	ӏB	��B	��B	�aB	�uB	ӏB	�[B	�&B	�TB	�B	��B	��B	�B	�B	�hB	��B	҉B	ϑB	ϑB	�B	��B	ңB	�TB	҉B	��B	�&B	�:B	��B	��B	��B	��B	��B	�1B	ȚB	��B	��B	ƨB	�mB	�9B	�	B	̳B	ɠB	��B	��B	�KB	�B	ȴB	��B	�B	�B	�HB	�.B	�bB	�B	��B	��B	��B	��B	ؓB	�KB	ںB	��B	��B	�B	�B	ߤB	�B	�B	��B	�TB	�B	�,B	�`B	�B	�2B	�B	�B	��B	�8B	�B	�LB	��B	�B	�LB	�LB	��B	�B	�@B	��B	�XB	��B	��B	�B	�B	�B	�>B	�B	�$B	��B	�B	�B	�B	��B	��B	�B	�DB	�B	�6B	�kB	��B	�]B	�qB	�)B	�]B	�B	�B	��B	��B	�B	�B	��B	�)B	�CB	�]B	�wB	�]B	�B	�iB	�B	�;B	��B	�[B	��B	�-B	��B	�B	��B	�ZB	�ZB	��B	�B	��B	�lB	��B	��B	��B	��B	�XB	��B	��B	�0B	�JB	�B	�B	��B	��B	��B	�qB	�wB	��B	��B	��B	��B	��B
  B
 4B	�}B	��B	��B
  B
 4B
�B
'B
[B
AB
�B
�B
�B
�B
�B
B
�B
�B
MB
�B
SB
�B
�B
?B
�B
�B
�B
B
�B
zB
�B
�B
�B
	�B

rB

�B

�B

�B
�B
B
dB
�B
6B
�B
�B
"B
�B
�B
�B
HB
�B
�B
B
�B
:B
�B
�B
@B
�B
�B
B
FB
{B
aB
aB
B
B
MB
2B
�B
gB
�B
�B
�B
mB
�B
�B
YB
�B
�B
�B
�B
�B
EB
+B
yB
�B
eB
�B
KB
KB
eB
eB
KB
B
�B
eB
�B
KB
eB
�B
KB
1B
QB
�B
WB
qB
qB
#B
�B
�B
B
xB
�B
VB
�B
�B
�B
pB
!B
�B
!-B
 vB
!�B
"4B
"4B
"NB
"�B
"�B
#B
#B
# B
#nB
#�B
#�B
#�B
#�B
#�B
$ZB
$�B
$�B
$�B
%FB
%zB
%zB
%�B
%zB
%�B
&�B
&�B
&2B
%�B
&LB
&�B
'B
'B
'mB
'�B
'�B
(
B
(XB
(XB
(sB
(�B
)B
)_B
)�B
)�B
*B
*B
*KB
*KB
+B
+�B
+�B
+�B
+�B
+�B
,"B
+�B
,WB
,WB
,�B
-)B
-�B
-�B
-]B
-�B
.B
.�B
/�B
0oB
0�B
0�B
1[B
1�B
1vB
2B
2-B
2GB
2�B
3�B
3�B
3�B
4B
4B
4B
4B
49B
4�B
4�B
4TB
4TB
4�B
4nB
4�B
4�B
5%B
5?B
5ZB
5ZB
5tB
5�B
5�B
5�B
6+B
6zB
6�B
6zB
6�B
7fB
7�B
8B
8B
7�B
8RB
8�B
8�B
9	B
8�B
8lB
9>B
9	B
9rB
9�B
9XB
9XB
9rB
9�B
9�B
9�B
9XB
9>B
9�B
9�B
9�B
:B
9�B
9�B
:*B
:DB
:�B
<B
<6B
;�B
;B
;�B
;�B
;dB
;dB
;dB
;dB
;JB
;B
;dB
<B
;�B
;�B
<PB
<�B
<�B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
>]B
?cB
?�B
?�B
@B
@�B
@�B
A B
AoB
A�B
BAB
BB
BAB
B[B
B[B
BuB
B�B
CaB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E9B
E9B
E9B
EmB
E�B
E�B
FB
F?B
FtB
F�B
F�B
F�B
GB
GEB
GzB
G�B
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
IlB
I�B
J	B
JrB
J�B
J�B
K)B
K�B
K�B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
MPB
MPB
M�B
M�B
M�B
N<B
NpB
N�B
N�B
OBB
O(B
OBB
O�B
O�B
OvB
O�B
O�B
P.B
PbB
P�B
P�B
QB
Q4B
QNB
QhB
QhB
Q�B
Q�B
R�B
S&B
S[B
S�B
S�B
S�B
TaB
T{B
T�B
T�B
T�B
T�B
U�B
V9B
VB
VmB
V�B
V�B
W$B
W
B
W
B
W?B
W�B
XB
W�B
W�B
W�B
XB
XyB
X�B
X�B
YKB
YB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\B
\]B
]/B
]IB
]dB
]IB
]B
]IB
]/B
]B
\�B
]/B
]IB
]�B
^B
^B
^B
^5B
^�B
_!B
_;B
_!B
_;B
_�B
_�B
_�B
`'B
`\B
`�B
aB
a|B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
c B
c B
c:B
c:B
cTB
c�B
c�B
d&B
d@B
d�B
d�B
e`B
e`B
eFB
e`B
e�B
e�B
ezB
e�B
e�B
e�B
ffB
gB
f�B
gB
gB
g8B
g8B
gmB
gmB
gmB
g�B
h
B
h>B
h>B
h$B
h�B
h�B
iB
h�B
h�B
i*B
h�B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jB
jB
j�B
kB
kB
k�B
k�B
l"B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
n�B
o B
oB
oOB
oiB
oOB
oiB
oiB
o�B
o�B
o�B
p!B
pUB
poB
p�B
p�B
q'B
qAB
qvB
q�B
q�B
raB
r|B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t9B
tnB
tnB
t�B
t�B
uB
uB
u%B
u?B
u?B
uZB
u�B
u�B
u�B
vB
vB
vB
v+B
v�B
v�B
v�B
w2B
wLB
wB
wLB
wfB
w�B
w�B
w�B
xB
w�B
x8B
xlB
x�B
x�B
x�B
y	B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
{B
z�B
z�B
{JB
{dB
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|jB
{�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111133  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,1<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910220046112019102200461120191022004611202207271133062022072711330620220727113306202207271535392022072715353920220727153539  JA  ARFMdecpA30a                                                                20191010093729  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191010093823  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191010093824  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191010093825  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191010093825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191010093825  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191010093825                      G�O�G�O�G�O�                JA  ARUP                                                                        20191010095455                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191011000000  CF  PSAL_ADJUSTED_QC@D�a�G�O�                JM  ARCAJMQC2.0                                                                 20191021154611  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191021154611  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023306  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063539  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                