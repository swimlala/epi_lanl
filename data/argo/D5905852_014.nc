CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-06-22T03:37:51Z creation;2019-06-22T03:37:53Z conversion to V3.1;2022-08-02T05:12:28Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190622033751  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_014                    2C  D   APEX                            8420                            2.11.2                          846 @��L:7_ 1   @��L����@--O�;dZ�dj��S&1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�33@���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�ffB���B�  B�33B�ffB���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���C  C�fC  C  C
  C33C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C633C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @   @��
@��A   A33AAp�A`��A��\A���A��\A��RA���A���A��A��B \)BQ�B\)BG�B G�B(\)B0\)B8\)B@\)BHffBPG�BX=qB`\)Bhp�Bpp�BxffB�.B�33B�
=B�8RB�#�B��B��B��B�#�B�G�B���B�\B�.B�W
B���B���B�.B�G�B�k�B�B�{B�(�B�#�B�(�B��B�#�B�.B�8RB�=qB�(�B��{B�(�B���C�C
=C�C�C
!HCW
C�qC�C
C�C
C\C\C{C\C {C"
C${C&�C(�C*{C,)C.�C0{C2
C45�C6B�C7�3C:�C<\C>{C@�CB)CD!HCF{CH)CJ\CL{CN�CP\CR�CT
=CV\CX�CZ�C\\C^
C`{Cb\Cd�Cf
Ch�Cj
Cl)Cn+�Cp(�Cr�Ct
Cv{Cx�Cz{C|�C~�C��C�
=C�
=C��C�
=C�
=C��C�
=C�C�C�
=C�
=C��C��C��C�\C�C��C��C��C��C��C�
=C��C��C��C�
C�C�\C��C�fC��C��C��C��C��C��C�
=C��C�fC��C��C��C��C��C�
=C��C��C��C�\C�C��C��C��C��C�
=C�
=C��C�
=C�
=C��C�C��C��C�\C�C��C�
=C��C��C��C�C�C�
=C��C��C�fC�fC��C�fC�fC��C��C��C�C�C��C��C��C��C��C�
=C��C��C��C�\C�C�
=C��C��C��C��C��C��C��C��C�
=C��C��C��C��C��C��C�\C��C��C�C�
=C�
=C�C��C�
=C��C�C�C��C�
=C��D 
D �
D
D��D�D��D�D�fD�D��DfD��D{D��D
D�fD�D�fD	
D	�{D
�D
�
D�D�
DRD�fDD�{D�D��D
D��DfD��DfD�{D�D�DD�{DD�
D
D��D{D�{D�D��D�D�fD{D�fDD�D�D�fD�D��D�D�D
D�fD
D��D fD ��D!{D!��D"�D"�{D#D#��D$
D$�D%�D%��D&�D&�D'
D'�{D(�D(��D)�D)�
D*RD*�
D+fD+��D,D,�D-�D-�fD.�D.�fD/D/�{D0�D0��D1{D1��D2�D2��D33D3�D4D4�D5�D5�fD6fD6�fD7�D7�{D8�D8�fD9{D9�D:{D:�3D;3D;��D<D<��D=D=�D>D>�D?fD?�fD@�D@��DA�DA�{DB{DB�DC
DC�
DD�DD�{DE{DE�DF
DF�fDG�DG�fDH{DH��DI�DI��DJ{DJ�DKfDK��DLfDL��DMDM�DNDN��DO�DO��DPfDP�
DQ
DQ��DR�DR��DS�DS�
DT�DT�DU�DU�3DV�DV�fDWDW��DXfDX�RDY�DY�fDZ
DZ�RD[fD[��D\{D\�fD]�D]�{D^�D^�
D_D_�{D`D`��Da�Da��Db{Db�Dc
Dc�
DdfDd�DeDe��DfDf�Dg
Dg�fDh
Dh��Di�Di�{DjDj�fDk
Dk��Dl
Dl�fDmDm�{Dn�Dn��Do{Do�{Dp�Dp��DqfDq��DrfDr��Ds�Ds��Dt3Dt��Du�Du��Dv�Dv�RDw
Dw�fDxDx�{Dy{Dy�fDz	�Dz��D{3D{��D|{D|�
D}D}��D~fD~�D{D��D�3D�B�D��=D���D��D�B�D���D�D��D�B�D���D���D��D�A�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D��=D��HD��D�B=D��=D���D��D�C3D���D�D��D�C�D��=D���D��D�C3D��)D��3D��D�A�D���D�D��D�A�D���D��3D��D�B�D���D�D�=D�B=D���D�D��D�A�D��=D��=D�3D�B�D���D�D��D�A�D��=D��3D��D�A�D��HD���D�)D�D)D���D�ÅD��D�C3D��=D��=D��D�C�D���D��3D��D�B�D���D���D�=D�B�D���D��3D��D�D)D���D���D�=D�B=D��=D���D��D�A�D��=D��=D��D�B�D���D�D�=D�A�D��3D�ÅD�3D�C3D��3D���D��D�B�D��3D���D��D�C�D���D��3D��D�B�D��3D�D��D�C3D���D�D��D�C3D���D�ÅD��D�C3D���D���D�3D�C3D���D���D��D�B�D���D�ÅD�3D�C3D��3D�D�=D�A�D���D���D��D�C�D��)D��3D��D�C�D��3D���D��D�C�D���D���D��D�B=D���D���D��D�B�D���D�ÅD��D�AHD���D���D��D�C�D���D���D��D�B�D���D���D�3D�B=D���D��{D�)D�C�D���D��HD��D�B�D���D���D�3D�A�D���D���D��D�C�D��3D��3D�3D�C3D���D���D��D�B=D���D�ÅD�)D�B�D��=D��3D��D�D)D��)D��3D�HD�B�D���D��=D��D�A�D��=D��3D��D�B�D��=D���D��D�A�D���D���D��D�B�D���D��=D��D�A�D���D�ÅD�3D�B�D���D���D�=D�B=D���D���D�3D�C3D���D���D��D�B�D=D���D��D�A�DÁ�D�D��D�C3Dă3D���D��D�B�DŃ3D�ÅD�)D�B�Dƃ3D���D�=D�A�Dǂ�D�D��D�B�DȂ�D�D�=D�B�DɃ3D���D��D�B�Dʂ=D�D��D�B�D˃�D��3D��D�C3D̂=D��=D��D�B=D͂�D�ÅD�3D�B�D΂�D�ÅD��D�A�Dς�D��3D��D�D{DЄ)D��3D��D�C�Dу3D�ÅD��D�C3D҂�D��3D�=D�A�Dӂ=D��=D��D�C3Dԃ�D���D��D�B�DՁ�D���D��D�B=Dց�D���D��D�C�Dׂ�D���D�3D�B=D؁�D�D��D�C�Dك�D�ÅD��D�C�Dڃ�D���D�=D�B�Dۃ3D��=D��D�A�D܀�D���D��D�D{D݄)D��3D��D�B=Dށ�D��HD�HD�B�D߃�D�ÅD��D�B=D���D��3D�3D�C�D�=D��=D�3D�B�D�3D�ÅD��D�C3D��D���D�3D�B=D䃅D��{D��D�B�D�3D�ÅD��D�C3D悏D��=D�3D�C�D��D�ÅD��D�B�D�3D��3D��D�C�D��D���D��D�B�DꂏD�D��D�B�D�=D���D�3D�C3D�3D��3D�=D�B�D탅D���D��D�B�DD��3D��D�B�D�3D���D��D�C�D���D�D��D�C3D��D���D��D�C3D�3D���D��D�B�D��D��=D��D�B�D��D�D��D�C3D���D���D��D�C�D���D��3D��D�C�D���D�D�=D�B�D���D�ÅD�3D�C3D���D���D�=D�D)D���D���D��D�D)D�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A���A���A���A��A��rA��/A��QA��}A۷Aۨ�Aۇ_A�]�A�T�A�S�A�A�A�=�A�9�A�49A�3�A�.�A�(XA�&LA�#�A�:A��A��A�� A��mAڲ�Aڍ�A��A׌A�S�A�4A�\)A�D3A·+A�AA�f2A��6A̟�A�(�A�� Aˣ�Aʟ�A�[�A�h�A��AÛ�A�!bA�>A��A��A�ںA�v�A��A�� A�h
A��A�v`A��aA��#A���A�ZQA��YA���A�&�A��>A��A�%�A���A�uZA�0!A�˒A���A���A��A���A�MA��A���A�1[A�}"A��A�s�A�u�A���A�*�Ay�Au��Ap�NAo[�An0�Al�Aj�-Ai�Ah:�A_��AX�]AR.IAO�*AL˒AI�_AI.IAH�AFt�AE��ADK�A?�4A=u�A;��A8��A5��A4`�A3ȴA3|�A38�A2�A2"hA2JA0��A/�bA.PHA-��A,��A+5�A*\�A)��A(��A(|�A'��A'JA&�A&�pA&�3A&�$A&X�A%8�A$|�A$N�A#��A#VA"3�A!��A ��A �vA Z�A�TAd�AaA_Ay>A�EA�*A  A�A�AHA�A&�AsA�A��A�fAL�A�zAV�A�A�CA�rA?�A��A�zA��A�)AO�AYAuA��ArGA=qAAX�AƨAa�A&A�AɆAP�A�A�PA�9Ax�AJ�A�A
��A
MA	��A	��A	c�A	~A�wA�AI�A�AںA�PAMA~�A1'A�DA��A�zAh
A��A�7A��Ag�A"hA�A�}A��AOA�AߤA��AC�A�A ѷA �A ��A ��A f�A :�@�IR@�S�@��@���@��X@�z@�D�@�+k@�($@�~@�	@��A@�� @�>�@��_@���@�@�-�@���@��@���@� �@�p�@�-w@��@�#:@�p�@��@�ѷ@�@�Xy@��@�(�@�^5@��5@��Z@�L�@��]@��
@�o@��r@繌@�iD@��@�I@�6@��@�}@�u@�u�@�˒@��@�L�@��@��I@�@ߴ�@�xl@ݮ�@ݓ�@�8�@ܲ�@�M@ۘ�@��/@ڽ<@چY@�7�@��@���@��@٠�@�rG@�J�@�/@�)_@�$t@��@ؾ�@�{�@�O@ח�@֩�@��@�#�@�]d@�;�@���@Ӡ'@�zx@�o @�hs@�S&@�@@Ҁ�@�x@�@эP@�B�@��@�x�@�Ĝ@�.�@ͤ@@�A @��/@̋D@�*�@˲-@˜@��@�-�@ɺ^@�w2@��K@���@��@�{�@�h
@�4@�6@�@�A�@�6z@�8�@���@�?@��@Ñh@�(�@¾�@�>B@���@�-w@���@��+@�Z�@�0U@�@��@��@��@���@�,=@���@�+@��@���@�z�@�~@��-@�e�@�5�@��z@�v�@�Ov@�G@���@�v`@���@���@�V@���@�+@���@��_@�z�@�3�@�t�@� \@�($@��@���@�C�@���@���@��W@�e�@�dZ@���@��@�s@��@�RT@��@��$@�-@���@�a@��H@�D�@�
�@��*@�|�@�g�@�F@���@�R�@�@��a@�_p@��@��@���@���@�{�@�8�@���@��O@�dZ@�	l@�ں@�~(@�D�@�@�ݘ@���@�4@�͟@�z�@�<�@��@��'@���@�Ft@��'@�C�@�!�@��M@���@�B[@��@��@��D@�^5@�_@��@���@�rG@�*0@�v�@�&�@�($@�$@�"h@�e@��@���@���@��-@��f@�C�@��[@���@�B[@��*@�4@�	l@��`@��[@��j@���@�M�@��@��g@���@��~@�X�@��@�}V@�!�@��Q@��V@�Q�@��@�ی@��j@���@�q@�Z@�A�@��@��k@�Y�@�4�@��@��@��4@�O@��j@��z@�X�@�&@��@���@��@���@��z@��@�I�@��@���@�X�@��<@�i�@�-@��W@���@�^�@��@��2@��9@��@�@���@���@��f@�x�@�hs@�Vm@�"�@���@��r@�Ov@��@���@��4@�H�@��_@�,=@��T@�ԕ@��@��X@�O@�� @���@��~@�b�@�!�@�v�@�~@���@��q@�w2@�m]@��@��@��@�~(@�H@���@���@�J�@��	@��|@���@��@���@�h
@�6�@��@!-@~��@~�1@~v�@~1�@}��@}�@|�.@|PH@{�@{��@{t�@{Mj@{'�@z�"@z�<@zn�@z&�@y�7@yB�@x��@xɆ@x�e@x4n@v��@v^5@v.�@u��@uX@t�@tc�@t>B@t/�@t(�@t%�@tG@siD@rں@r�L@r	@q�X@p�[@p�D@o�6@oo@nv�@n!�@m�T@m��@mzx@l��@l,=@k�6@k��@kE9@j��@j}V@j3�@i��@im]@h�K@hw�@h7�@hM@g�
@g��@g@fn�@f�@e��@e+�@dĜ@dM@c�q@c�@b�M@b�1@bd�@b�@a�z@a��@a^�@a�@`�`@`�@`[�@`~@_ƨ@_
=@^�8@^�r@^�@]�d@]�S@]`B@\��@\��@[�@[�@Z�R@ZkQ@Z5?@Y��@X�@X��@X1@Wƨ@Ws@V��@V��@VM�@U�.@U�H@Uk�@T��@T,=@T�@S�@So�@R��@R �@QY�@P��@PZ@P2�@O�+@OE9@Nں@N��@N_�@M��@MY�@L��@L@K�@K6z@J��@J��@J:*@I�d@I�@Is�@Iw2@Ia�@I@@H��@H��@HH@G�@@G&@F�!@F)�@E��@E��@E�@E�@D��@Dh�@C�@C9�@C"�@C�@B�@B�\@B.�@A�@AQ�@AA @A/@@��@@`�@@$@?�@@?y�@?.I@>�y@>�x@>4@=��@=hs@=IR@=5�@=%F@<��@<'R@;;d@:҉@:p;@:�@9�D@9�)@9�@9��@9+@8��@8_@8:�@8�@81@7�6@7��@7~�@7=@6_�@5�@5�S@5�M@5(�@4��@4|�@4N�@4�@3�A@3�m@3�*@3�@3\)@3Y@2�6@2q�@2Z�@2V@2V@2+k@1�>@1�@1(�@1�@0�E@0�O@0��@0��@0tT@0_@0M@0(�@/�&@/&@.�h@.��@.W�@.O@._@.�@-�#@-�h@-Q�@-�@,��@,�_@,tT@,c�@,,=@+�K@+�q@+�	@+W?@+C�@+�@*��@*�@*5?@)�D@)��@)k�@)A @)@)	l@(�/@(��@(g8@(,=@(1@'�@'�g@'��@'�[@'�:@'s@'RT@'"�@&��@&�8@&�@&�@&�!@&� @&_�@&1�@&{@%�o@%ϫ@%��@%!�@$�@$��@$��@$�9@$��@$�z@$H@$*�@$�@$�@#�@#��@#��@#o@"��@"�R@"i�@"{@!�@!��@!s�@!�@ �@ �Y@ V�@ M@ <�@ M@��@n/@+@҉@��@�@R�@)�@ �@�d@�h@/@��@��@(�@�W@�k@@O@�@͟@�1@^5@_@��@IR@+�@�@�@h�@�]@� @�$@]�@@d�@;�@$�@u@��@�@�@p�@[W@(�@�P@��@[�@I�@,=@��@�;@��@�k@�f@j�@E9@
=@�@��@�X@�R@�\@i�@Q@	@�#@�t@zx@hs@%F@��@��@�@u�@S�@,=@@�r@�m@�@�@ƨ@dZ@'�@C@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A���A���A���A��A��rA��/A��QA��}A۷Aۨ�Aۇ_A�]�A�T�A�S�A�A�A�=�A�9�A�49A�3�A�.�A�(XA�&LA�#�A�:A��A��A�� A��mAڲ�Aڍ�A��A׌A�S�A�4A�\)A�D3A·+A�AA�f2A��6A̟�A�(�A�� Aˣ�Aʟ�A�[�A�h�A��AÛ�A�!bA�>A��A��A�ںA�v�A��A�� A�h
A��A�v`A��aA��#A���A�ZQA��YA���A�&�A��>A��A�%�A���A�uZA�0!A�˒A���A���A��A���A�MA��A���A�1[A�}"A��A�s�A�u�A���A�*�Ay�Au��Ap�NAo[�An0�Al�Aj�-Ai�Ah:�A_��AX�]AR.IAO�*AL˒AI�_AI.IAH�AFt�AE��ADK�A?�4A=u�A;��A8��A5��A4`�A3ȴA3|�A38�A2�A2"hA2JA0��A/�bA.PHA-��A,��A+5�A*\�A)��A(��A(|�A'��A'JA&�A&�pA&�3A&�$A&X�A%8�A$|�A$N�A#��A#VA"3�A!��A ��A �vA Z�A�TAd�AaA_Ay>A�EA�*A  A�A�AHA�A&�AsA�A��A�fAL�A�zAV�A�A�CA�rA?�A��A�zA��A�)AO�AYAuA��ArGA=qAAX�AƨAa�A&A�AɆAP�A�A�PA�9Ax�AJ�A�A
��A
MA	��A	��A	c�A	~A�wA�AI�A�AںA�PAMA~�A1'A�DA��A�zAh
A��A�7A��Ag�A"hA�A�}A��AOA�AߤA��AC�A�A ѷA �A ��A ��A f�A :�@�IR@�S�@��@���@��X@�z@�D�@�+k@�($@�~@�	@��A@�� @�>�@��_@���@�@�-�@���@��@���@� �@�p�@�-w@��@�#:@�p�@��@�ѷ@�@�Xy@��@�(�@�^5@��5@��Z@�L�@��]@��
@�o@��r@繌@�iD@��@�I@�6@��@�}@�u@�u�@�˒@��@�L�@��@��I@�@ߴ�@�xl@ݮ�@ݓ�@�8�@ܲ�@�M@ۘ�@��/@ڽ<@چY@�7�@��@���@��@٠�@�rG@�J�@�/@�)_@�$t@��@ؾ�@�{�@�O@ח�@֩�@��@�#�@�]d@�;�@���@Ӡ'@�zx@�o @�hs@�S&@�@@Ҁ�@�x@�@эP@�B�@��@�x�@�Ĝ@�.�@ͤ@@�A @��/@̋D@�*�@˲-@˜@��@�-�@ɺ^@�w2@��K@���@��@�{�@�h
@�4@�6@�@�A�@�6z@�8�@���@�?@��@Ñh@�(�@¾�@�>B@���@�-w@���@��+@�Z�@�0U@�@��@��@��@���@�,=@���@�+@��@���@�z�@�~@��-@�e�@�5�@��z@�v�@�Ov@�G@���@�v`@���@���@�V@���@�+@���@��_@�z�@�3�@�t�@� \@�($@��@���@�C�@���@���@��W@�e�@�dZ@���@��@�s@��@�RT@��@��$@�-@���@�a@��H@�D�@�
�@��*@�|�@�g�@�F@���@�R�@�@��a@�_p@��@��@���@���@�{�@�8�@���@��O@�dZ@�	l@�ں@�~(@�D�@�@�ݘ@���@�4@�͟@�z�@�<�@��@��'@���@�Ft@��'@�C�@�!�@��M@���@�B[@��@��@��D@�^5@�_@��@���@�rG@�*0@�v�@�&�@�($@�$@�"h@�e@��@���@���@��-@��f@�C�@��[@���@�B[@��*@�4@�	l@��`@��[@��j@���@�M�@��@��g@���@��~@�X�@��@�}V@�!�@��Q@��V@�Q�@��@�ی@��j@���@�q@�Z@�A�@��@��k@�Y�@�4�@��@��@��4@�O@��j@��z@�X�@�&@��@���@��@���@��z@��@�I�@��@���@�X�@��<@�i�@�-@��W@���@�^�@��@��2@��9@��@�@���@���@��f@�x�@�hs@�Vm@�"�@���@��r@�Ov@��@���@��4@�H�@��_@�,=@��T@�ԕ@��@��X@�O@�� @���@��~@�b�@�!�@�v�@�~@���@��q@�w2@�m]@��@��@��@�~(@�H@���@���@�J�@��	@��|@���@��@���@�h
@�6�@��@!-@~��@~�1@~v�@~1�@}��@}�@|�.@|PH@{�@{��@{t�@{Mj@{'�@z�"@z�<@zn�@z&�@y�7@yB�@x��@xɆ@x�e@x4n@v��@v^5@v.�@u��@uX@t�@tc�@t>B@t/�@t(�@t%�@tG@siD@rں@r�L@r	@q�X@p�[@p�D@o�6@oo@nv�@n!�@m�T@m��@mzx@l��@l,=@k�6@k��@kE9@j��@j}V@j3�@i��@im]@h�K@hw�@h7�@hM@g�
@g��@g@fn�@f�@e��@e+�@dĜ@dM@c�q@c�@b�M@b�1@bd�@b�@a�z@a��@a^�@a�@`�`@`�@`[�@`~@_ƨ@_
=@^�8@^�r@^�@]�d@]�S@]`B@\��@\��@[�@[�@Z�R@ZkQ@Z5?@Y��@X�@X��@X1@Wƨ@Ws@V��@V��@VM�@U�.@U�H@Uk�@T��@T,=@T�@S�@So�@R��@R �@QY�@P��@PZ@P2�@O�+@OE9@Nں@N��@N_�@M��@MY�@L��@L@K�@K6z@J��@J��@J:*@I�d@I�@Is�@Iw2@Ia�@I@@H��@H��@HH@G�@@G&@F�!@F)�@E��@E��@E�@E�@D��@Dh�@C�@C9�@C"�@C�@B�@B�\@B.�@A�@AQ�@AA @A/@@��@@`�@@$@?�@@?y�@?.I@>�y@>�x@>4@=��@=hs@=IR@=5�@=%F@<��@<'R@;;d@:҉@:p;@:�@9�D@9�)@9�@9��@9+@8��@8_@8:�@8�@81@7�6@7��@7~�@7=@6_�@5�@5�S@5�M@5(�@4��@4|�@4N�@4�@3�A@3�m@3�*@3�@3\)@3Y@2�6@2q�@2Z�@2V@2V@2+k@1�>@1�@1(�@1�@0�E@0�O@0��@0��@0tT@0_@0M@0(�@/�&@/&@.�h@.��@.W�@.O@._@.�@-�#@-�h@-Q�@-�@,��@,�_@,tT@,c�@,,=@+�K@+�q@+�	@+W?@+C�@+�@*��@*�@*5?@)�D@)��@)k�@)A @)@)	l@(�/@(��@(g8@(,=@(1@'�@'�g@'��@'�[@'�:@'s@'RT@'"�@&��@&�8@&�@&�@&�!@&� @&_�@&1�@&{@%�o@%ϫ@%��@%!�@$�@$��@$��@$�9@$��@$�z@$H@$*�@$�@$�@#�@#��@#��@#o@"��@"�R@"i�@"{@!�@!��@!s�@!�@ �@ �Y@ V�@ M@ <�@ M@��@n/@+@҉@��@�@R�@)�@ �@�d@�h@/@��@��@(�@�W@�k@@O@�@͟@�1@^5@_@��@IR@+�@�@�@h�@�]@� @�$@]�@@d�@;�@$�@u@��@�@�@p�@[W@(�@�P@��@[�@I�@,=@��@�;@��@�k@�f@j�@E9@
=@�@��@�X@�R@�\@i�@Q@	@�#@�t@zx@hs@%F@��@��@�@u�@S�@,=@@�r@�m@�@�@ƨ@dZ@'�@C@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	:^B	:DB	:DB	:xB	:�B	:�B	:xB	9>B	9>B	9>B	7�B	7�B	8B	7�B	7�B	7�B	7LB	7LB	72B	7�B	7�B	7�B	7�B	7�B	8�B	9�B	:*B	:B	:*B	9�B	8�B	5�B	*B	tB߾B�B�TB	 �B	@�B	YeB	x�B	��B	��B	�tB	�iB	�B	�lB
U�B
��B
��B
�B%,B1'B9�B>�BJ�B^�Bs�B|6B�B��B�HB�B��B��BuBU2BT{Be�B[=BKB7B%B%�B(>B%FB�B
�KB
ևB
��B
� B
{�B
qvB
d&B
BB
/5B
�B
B
�B	�B	��B	�B	�\B	��B	�HB	��B	� B	}<B	z*B	Y�B	9�B	-�B	2|B	5�B	7fB	7�B	:B	=�B	<jB	>�B	=�B	6zB	BB	J�B	N�B	R�B	Z7B	^�B	iyB	� B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�UB	��B	�B	�8B	��B	��B	�}B	�iB	��B	�B	�xB	�6B	�6B	҉B	��B	ݲB	��B	�OB	��B
 �B
�B
?B
fB
	B
jB
(B
�B
vB
uB

B
�B
�B
�B
�B
B
 �B
%�B
$�B
#�B
(
B
)B
)*B
(�B
(sB
)B
+kB
-]B
-�B
*�B
+kB
+�B
+�B
,�B
,B
+�B
*�B
,�B
33B
2�B
2�B
2�B
3�B
3B
2�B
2�B
2|B
2B
1vB
2-B
1�B
1�B
1�B
2�B
3hB
3�B
3�B
4B
5%B
5tB
4B
49B
3�B
3MB
2�B
2�B
2|B
2�B
2�B
2�B
3hB
2aB
2GB
2B
1�B
1'B
1[B
1'B
0�B
1B
0�B
0�B
0�B
0UB
0;B
0B
/�B
.�B
/�B
.cB
-�B
,�B
,"B
+�B
+�B
+�B
+�B
+�B
+kB
+6B
+B
*�B
)�B
)yB
(�B
(>B
'�B
'mB
'B
&�B
&�B
%�B
%`B
$�B
#�B
#�B
#TB
#:B
"�B
"NB
!�B
!B
�B
B
jB
�B
/B
)B
�B
�B
#B
�B
�B
�B
eB
+B
�B
�B
gB
2B
�B
�B
{B
aB
B
�B
�B
�B
�B
B
�B
oB
�B
hB
hB
4B
 B
�B
�B
�B
�B
�B
bB
HB
.B
B
�B
\B
�B
B
~B
�B
xB
DB
^B
�B
�B
B
0B
�B
0B
0B
B
�B
xB
B

�B
DB

�B

=B
	lB
�B
�B
zB
+B
�B
�B
?B
?B
B
�B
�B
�B
{B
�B	��B
UB
 iB
�B
�B
 OB
UB
�B
gB
�B
�B
B
�B
�B
�B
^B
�B
�B
B
�B
dB
dB
JB
0B
�B
�B

=B
	lB
�B
�B
�B
�B
	B
�B
KB
�B
�B
YB
B
B
�B
BB
�B
�B
B
jB

=B
B
dB
jB
PB
�B

�B
�B
�B
	B
�B
�B
�B
?B
B
	7B
�B
�B
}B
�B
�B
 B
�B
bB
BB
�B
�B
�B
\B
�B
�B
B
@B
�B
,B
�B
�B
B
�B
�B
�B
aB
,B
�B
�B
�B
�B
\B
vB
B
.B
HB
�B
�B
B
�B
�B
:B
�B
�B
@B
�B
{B
�B
2B
MB
MB
�B
�B
�B
EB
EB
�B
�B
yB
�B
kB
qB
qB
qB
�B
qB
qB
qB
�B
qB
qB
�B
�B
�B
�B
�B
�B
B
B
/B
IB
/B
B
�B
B
B
OB
OB
�B
�B
�B
 �B
 �B
!B
!�B
!�B
"B
"B
"�B
"�B
"hB
"hB
"�B
#�B
#�B
$B
#�B
$B
$tB
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'8B
'8B
'�B
(>B
(XB
(�B
(�B
)B
)*B
)�B
)_B
)_B
*0B
*KB
*B
*B
*B
*B
*B
*B
*�B
*�B
+�B
+�B
,"B
,WB
,=B
,qB
-B
-�B
.B
-�B
-�B
-�B
-�B
/ B
/�B
/�B
0B
0!B
1AB
1�B
1�B
2GB
2�B
2|B
33B
3hB
3�B
4nB
4�B
5?B
5�B
6FB
6�B
6zB
6`B
6`B
6�B
7B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
9$B
9>B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
:DB
:DB
:DB
:DB
9�B
:xB
;0B
;0B
;B
;0B
;�B
<B
<jB
<jB
<jB
<jB
<PB
<6B
<�B
<�B
<�B
="B
=<B
=qB
=�B
>]B
>�B
>�B
?cB
?HB
?HB
?.B
?�B
?�B
@ B
@4B
@OB
@�B
@�B
AB
AoB
AUB
A�B
BB
BAB
BAB
BuB
B�B
CGB
C�B
C�B
C�B
DgB
D�B
E�B
F%B
FtB
FYB
F�B
F�B
F�B
GB
GB
GEB
GzB
GzB
G�B
G�B
G�B
G�B
H�B
H�B
IB
IRB
I�B
I�B
J	B
JrB
J=B
J�B
J�B
JrB
K)B
KxB
K�B
K�B
K^B
K�B
K�B
K�B
L0B
L~B
MPB
M�B
M�B
M�B
N<B
M�B
N"B
N"B
NB
N<B
N�B
OvB
O�B
O�B
O�B
O�B
PHB
PbB
P}B
P}B
P�B
QhB
Q�B
Q�B
R B
R�B
R�B
R�B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T,B
T,B
TaB
T�B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
V�B
W
B
W?B
W�B
W�B
W�B
XB
X_B
X�B
X�B
Y1B
YB
YB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
[	B
[WB
[�B
\)B
\]B
\]B
\]B
\]B
\CB
\�B
]dB
]�B
^5B
^OB
^jB
^jB
^OB
^jB
^�B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a-B
a|B
a�B
a�B
b4B
bNB
bNB
b�B
b�B
b�B
cB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
fB
fLB
fLB
f�B
f�B
ffB
f�B
g8B
gmB
gmB
gRB
g�B
g�B
g�B
h
B
hsB
hXB
h�B
h�B
h�B
h�B
h�B
iB
i_B
i�B
i�B
j0B
jeB
jB
jB
j�B
j�B
kB
k6B
k6B
kkB
kQB
kkB
k�B
k�B
k�B
k�B
lB
l"B
l"B
lB
lB
l=B
l=B
lqB
l�B
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nB
n/B
nIB
n}B
n}B
n�B
o5B
o�B
o�B
p;B
poB
poB
pUB
pUB
p�B
q'B
qAB
qvB
qvB
qvB
q�B
rB
rGB
r|B
r�B
r�B
sB
shB
sMB
s�B
s�B
s�B
t9B
tTB
t�B
u%B
u?B
u�B
vB
v+B
v`B
vzB
vzB
v�B
v�B
w�B
w�B
xB
x�B
x�B
y$B
y>B
yrB
y�B
y�B
zDB
zDB
z�B
z�B
z�B
z�B
{0B
{0B
{0B
{dB
{JB
{dB
{B
{�B
|B
|�B
|�B
|�B
}B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~BB
~wB
~BB
~BB
~B
}�B
~(B
~BB
~]B
~wB
~�B
~�B
~�B
~�B
.B
.B
B
~�B
B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	:xB	:^B	:DB	:xB	:�B	:�B	:�B	9XB	9rB	9rB	8B	7�B	8RB	7�B	7�B	7�B	7fB	7fB	7LB	7�B	7�B	7�B	7�B	8B	8�B	9�B	:^B	:DB	:xB	:^B	9�B	8�B	/5B	�B��B�B�B	"NB	BB	Z�B	y�B	�_B	��B	�+B	��B	��B	�VB
Y�B
�B
�B
��B'B2�B;JBAoBM�BabBt�B}�B��B�NB��B�MB�dB��B}�B]�BZ�BhsB^5BN�B9�B%�B&�B*B(�BB
��B
�#B
�B
��B
~�B
u�B
kkB
FtB
4B
�B
{B
<B	��B	ӏB	��B	�hB	��B	��B	��B	�AB	��B	��B	bhB	@�B	1'B	6B	8�B	88B	9XB	<B	?B	?.B	C�B	@�B	9�B	E�B	M�B	PbB	S@B	Z�B	_!B	j0B	��B	�6B	�oB	��B	��B	�B	��B	�B	��B	��B	�'B	��B	�B	��B	��B	��B	�}B	��B	��B	�mB	�JB	͹B	�<B	�[B	�B	ބB	�B	�B	�^B
�B
�B
zB
	�B

#B
"B
�B
�B
�B
�B
sB
YB
�B
�B
dB
�B
 �B
%�B
%zB
#�B
(sB
)DB
)_B
)_B
(�B
)*B
+�B
./B
./B
+B
+�B
+�B
,qB
-)B
,�B
,�B
+�B
.B
3hB
33B
3MB
3�B
3�B
3MB
2�B
2�B
2�B
2aB
2B
2�B
1�B
2B
2aB
2�B
3�B
3�B
4B
4TB
5�B
5�B
4�B
4�B
3�B
3�B
33B
2�B
2�B
3MB
3�B
3�B
3�B
2�B
2�B
2aB
2B
1[B
1�B
1[B
1'B
1vB
0�B
0�B
0�B
0UB
0oB
0UB
0;B
/�B
0;B
/iB
.IB
-B
,=B
,B
+�B
+�B
+�B
+�B
+�B
+kB
+kB
+6B
*B
*B
)_B
(�B
($B
'�B
'�B
'8B
&�B
&LB
%�B
$�B
$B
#�B
#�B
#nB
#:B
"�B
"4B
!�B
 BB
�B
�B
jB
B
�B
)B
�B
WB
#B
�B
QB
B
�B
_B
$B
�B
gB
2B
B
�B
�B
�B
�B
B
�B
�B
uB
@B
�B
�B
�B
�B
NB
 B
 B
 B
�B
�B
�B
bB
HB
HB
HB
�B
�B
\B
�B
B
dB
�B
xB
xB
�B
B
0B
JB
�B
~B
�B
JB
�B
�B
^B
�B
�B
)B

�B
	�B
�B
1B
�B
_B
B
�B
�B
�B
YB
�B
_B
�B
�B
�B
  B
�B
 iB
B
-B
 OB
UB
3B
�B
MB
�B
EB
�B
�B
0B
�B
�B
�B
0B
�B
dB
dB
~B
�B
B
�B

�B
	�B
�B
�B
�B
�B
	lB
�B
�B
�B
+B
�B
1B
PB
�B
�B
�B
B
vB
�B

rB

�B
�B
�B
�B
B
)B
�B
�B
	lB
�B
KB
EB
�B
B
	B
�B
B
bB
�B
�B
oB
 B
�B
�B
�B
<B
B
�B
�B
B
@B
�B
,B
FB
FB
B
2B
B
�B
�B
{B
{B
B
[B
�B
�B
vB
�B
.B
HB
bB
�B
B
NB
�B
 B
TB
:B
�B
�B
�B
�B
B
gB
�B
�B
B

B
B
yB
yB
�B
�B
�B
�B
�B
�B
qB
WB
qB
qB
qB
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
/B
IB
IB
dB
�B
B
B
OB
�B
�B
VB
 BB
 �B
 �B
!-B
!�B
"B
"4B
"B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$B
#�B
$ZB
$�B
%�B
%�B
&2B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'mB
'�B
'�B
(XB
(�B
(�B
)*B
)DB
)_B
)�B
)�B
)�B
*eB
*KB
*�B
*�B
*�B
*B
*�B
*�B
*�B
+B
+�B
+�B
,WB
,qB
,�B
,�B
-CB
.B
.B
-�B
-�B
-�B
.cB
/�B
0!B
0B
0;B
0�B
1vB
1�B
2-B
2aB
2�B
2�B
3MB
3hB
3�B
4�B
5%B
5tB
5�B
6`B
6�B
6`B
6zB
6zB
6�B
72B
7LB
7fB
7�B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
9$B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
:B
:^B
:DB
:DB
:^B
:DB
:�B
;0B
;0B
;0B
;dB
;�B
<6B
<jB
<PB
<jB
<�B
<jB
<jB
<�B
<�B
<�B
=<B
=�B
=�B
=�B
>�B
?B
?B
?}B
?cB
?cB
?cB
?�B
@ B
@B
@4B
@�B
@�B
AB
A B
A�B
A�B
BB
B'B
B[B
BAB
B�B
B�B
CaB
C�B
C�B
C�B
D�B
D�B
E�B
F?B
FtB
FtB
F�B
F�B
F�B
G+B
G+B
G_B
GzB
G�B
G�B
G�B
G�B
G�B
H�B
H�B
IB
IlB
I�B
I�B
J=B
J�B
J�B
J�B
J�B
JrB
KDB
K�B
K�B
K�B
K�B
LB
K�B
K�B
LJB
L�B
MjB
M�B
M�B
M�B
NVB
NB
N"B
N<B
N<B
N�B
N�B
O�B
PB
O�B
O�B
P.B
P}B
P}B
P�B
P�B
Q B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T{B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
W$B
WYB
W�B
W�B
W�B
XEB
X_B
X�B
X�B
Y1B
Y1B
YKB
YKB
Y�B
ZB
ZB
ZkB
Z�B
[#B
[qB
[�B
\CB
\]B
\xB
\xB
\xB
\xB
\�B
]�B
]�B
^5B
^OB
^OB
^�B
^jB
^�B
^�B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
`�B
`�B
aB
`�B
aHB
a�B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
cB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
eB
fB
f2B
ffB
ffB
f�B
f�B
ffB
f�B
g8B
g�B
g�B
gRB
g�B
g�B
g�B
h
B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
j0B
jB
jB
j�B
j�B
j�B
kB
k6B
k6B
kQB
k6B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l"B
l"B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
mB
mCB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
nB
n/B
nIB
n�B
n}B
n�B
oOB
o�B
pB
p;B
p�B
p�B
pUB
p�B
qB
q'B
q[B
q[B
qvB
qvB
q�B
r-B
rGB
r|B
r�B
r�B
s3B
s�B
sMB
s�B
s�B
s�B
t9B
tnB
t�B
u%B
uZB
u�B
v+B
v+B
v`B
vzB
v�B
v�B
v�B
w�B
xB
xRB
x�B
x�B
y$B
yXB
y�B
y�B
y�B
z^B
z^B
z�B
z�B
z�B
z�B
{JB
{0B
{0B
{B
{dB
{dB
{B
{�B
|6B
|�B
|�B
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}qB
}�B
}�B
~BB
~]B
~]B
~�B
~]B
~]B
~(B
}�B
~BB
~]B
~]B
~wB
~�B
~�B
~�B
~�B
B
.B
B
~�B
.B
�B
�B
�B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.09(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201907030050462019070300504620190703005046202207271131392022072711313920220727113139202207271534202022072715342020220727153420  JA  ARFMdecpA30a                                                                20190622033738  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190622033751  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190622033752  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190622033753  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190622033753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190622033753  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190622033753                      G�O�G�O�G�O�                JA  ARUP                                                                        20190622035509                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190623000000  CF  PSAL_ADJUSTED_QC@=q@=qG�O�                JM  ARCAJMQC2.0                                                                 20190702155046  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190702155046  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023139  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063420  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                