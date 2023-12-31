CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:21Z creation;2019-05-23T10:00:23Z conversion to V3.1;2022-08-02T05:12:53Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100021  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_005                    2C  D   APEX                            8420                            2.11.2                          846 @ذ�d��1   @ذ�ʆB @+��ݗ��d�����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  @���A   A@  A`  A�  A���A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BZ  B`  BfffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffBÙ�B�  B�  B�  B�  B�33B���B�ffB�ffB���B���B���B�  B�  B�  C �C  C  C  C  C
  C�C�C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@}p�@��@���A
=A@  A_\)A�A�{A��
A�{A��A��
A߅AA��BB�
B�B�
B'��B/��B8  B?��BGBO�HBZ
=B_�BfQ�BoBwB�
B��HB��fB��HB���B��)B��fB��B��B��B���B��B���B��B��B�B�aHBÔ{B���B��HB��fB��fB��B�B�aHB�W
B�B�qB���B��)B��B�C �C�qC�3C�3C�C	��C�C{C�fC�C��C�RC��C�qC�RC�RC�3C!�C#�3C%�C'�C)��C+��C-�C/��C2  C3��C5�C7�C9��C<  C>\C?�RCA�CC�fCE�CG�CI�CK�CM�3CO��CQ�RCS�qCU��CW��CY�C[�C]�3C_�Ca�3Cc�Ce�Cg�Ci�3Cl�Cm��Co�Cq�3Cs��Cu��Cw�RCy��C{��C}��C��C��qC���C���C��)C��)C���C��RC��
C���C���C���C���C��)C���C��)C���C���C��)C��)C���C��qC��)C���C��)C���C��
C��
C��)C�  C��)C��RC��)C���C��RC���C���C��RC��qC���C��)C��qC�  C��)C��qC��C�  C���C��RC���C��RC���C���C���C��qC��RC���C���C��RC��
C���C���C���C��)C�  C���C��)C���C���C��
C��
C���C��)C���C���C���C���C��qC���C���C���C��qC���C��qC��)C��)C��qC�  C��qC���C���C���C��RC���C��RC���C��)C�  C��qC���C���C��
C��RC��qC���C���C���C���C��RC���C���C���C���C�  C���C���C��qC��qC��C��C���C��)C���C���C��
C��)C��qC��)C��)D ~D �qD|)D��D|�D�qD|)D��D|)D��D\D�qD{�D�)D{�D��D\D�\D	}qD	��D
{�D
��D{�D��D~�D��D|�D�qD~�D��D~�D�D|�D�qD|�D��D{�D�)D|�D��D|)D��D}qD��D{�D�)D~D��D~D��D~D��D}qD�qD|�D�qD~�D��D}qD�)D|)D��D|)D��D ~D �D!~�D!��D"}qD"�D#�HD$  D$|�D$�qD%|�D%��D&|)D&��D'\D'��D(~�D(�qD)|�D)��D*{�D*��D+|�D+�qD,}qD,��D-|)D-��D.~�D/  D/~�D/��D0}qD0�qD1� D1��D2|�D2��D3}qD3�qD4~�D5 �D5~D5�)D6~D7  D7� D7�D8}qD8��D9� D9�qD:|)D:�qD;|�D;��D<}qD<��D=|)D=�)D>|)D>�)D?|�D?�D@}qD@�DA\DA��DB|�DB�qDC~�DC��DD{�DD�qDE\DE�DF}qDF��DG� DH�DH~DH�qDI~DI��DJ}qDJ�DK~�DK��DL~�DL�qDM~DM�qDN}qDN�)DO}qDO�DP}qDP�qDQ{�DQ��DR~�DR�\DS~�DS�)DT|)DT�DU~DU��DV}qDV�)DW|�DW�)DX}qDX��DY~�DY�DZ|�DZ��D[~�D[��D\|)D\�D]|)D]��D^|)D^�qD_��D_�D`|�D`�\Da\Da��Db~Db�qDc{�Dc��Dd}qDd�De~De�)Dfz�Df�)Dg~�Dg��Dh~�Dh��Di~�Di�Dj}qDk  Dk~�Dk�Dl}qDl��Dm~�Dm�qDn|�Dn�Do|�Do��Dp|�Dp�qDq|�Dq�)Dr~Ds  Ds~Ds��Dt\Dt��Du|�Du��Dv}qDv�)Dw}qDw�qDx\Dy �Dy\Dy�Dz|�Dz�D{~�D{�qD|~�D|��D}z=D}��D~|)D~��D|�D��D�>�D���D���D��\D�?
D�
D��\D���D�>D�~fD��
D��fD�>D�
D��
D���D�?
D�~�D���D��fD�>fD�\D���D��fD�>�D�~fD���D��D�?\D�~�D��fD��fD�>�D�
D���D��fD�>�D�~fD��fD��\D�?
D�
D��D��qD�?
D�~fD��D���D�?�D��D��\D��\D�?�D�~�D���D��fD�?
D�~D��D���D�?
D�~�D��
D���D�?
D�\D��fD���D�?
D�~D���D��
D�>fD�~D��D��D�=�D�}�D��D��fD�?
D�� D��\D��
D�>fD�}�D���D���D�?
D�\D��\D���D�@ D�\D��fD���D�?
D�~�D��
D���D�>fD�~fD��
D��fD�>fD�
D��
D��fD�>fD�
D��
D���D�>fD�~D��fD��
D�?
D�\D��
D��D�?
D�
D��fD���D�>�D�
D��fD���D�>D�~D���D��D�>�D��D���D��fD�>D�~fD��fD���D�=�D�}D��D��fD�?�D��D���D��D�>�D�~fD��D��fD�?
D�� D���D��
D�>�D�~�D��
D��
D�?
D�
D��\D��fD�<�D�~fD��fD��D�=�D�}�D��D���D�?\D�~D��qD���D�?\D�}�D���D��
D�?�D��RD��
D���D�?\D�
D���D��D�=�D�~D��fD��D�?
D��D���D��D�>D�}�D���D��
D�>�D�
D���D���D�>fD�}qD��D���D�>�D�
D��
D���D�?�D��D���D��
D�?\D�~fD���D���D�@ D�\D��
D��
D�>�D��D��
D��
D�>�D�~D��fD��
D�>�D�~fD��
D��
D�>fD�}D��D��
D�?
D�\D��\D���D�>�D�~fD���D��qD�>�D�
D���D��qD�=qD�~�D��fD��D�>D�~D��\D���D�>fD�\D���D��\D�?
D�~D¾�D��\D�?�D�~�DþD��
D�@RDĀ DĿ�D���D�=qD�~�Dſ�D���D�?�D�
Dƾ�D��fD�>D�~�DǾfD���D�=�D�}qDȽ�D��D�=�D�}qDɾD��D�>D�~DʾfD��
D�>fD�}�D˾fD��
D�>D�}�D̽qD��fD�@ D̀ D;�D��fD�>fD�
Dξ�D��D�>D�
DϾ�D��D�=�D�~fDп\D���D�?
D�
DѾD��D�>D�~�Dҿ
D��fD�>�D�}�Dӽ�D��fD�?
D�~�DԿ
D���D�?�D�
Dտ
D���D�=qD�}qDֿ
D��\D�?\D�~fD׿\D��
D�>fD��Dؿ
D��D�=qD�~Dٿ
D���D�=qD�~�Dڿ
D���D�>�D�
D۾�D���D�=�D�}Dܾ�D��
D�>D�
DݾfD��D�>fD�}�D޽qD��qD�>�D�\D߿
D��fD�?
D�\D�D��
D�@ D�\D�\D��\D�?\D��D�\D��D�=D�}qD�qD���D�>fD�~�D侸D���D�?�D�
D�fD��\D�>�D�~fD�
D��fD�>D�\D���D� �D�>�D�~fD�D��qD�=�D�
D�\D��fD�?
D�
D�fD���D�>fD�~�D뾸D��D�>�D��D�fD��qD�>�D�\D��
D��
D�>�D�~fD�
D���D�>D�~�DﾸD��qD�>fD��D�D���D�>�D�~D�D��\D�>fD�
D�\D��
D�?
D��D�\D��\D�@ D�\D���D��
D�@ D��D���D��D�>D�~fD���D��\D�?\D�~�D��
D���D�>fD�\D���D���D�?
D��D���D���D�?
D�~fD��fD��fD�>�D�
D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�`�A�c A�e`A�o5A�qAA�r�A�s�A�s�A�tTA�s�A�tTA�tA�u�A�x8A�y�A�zxA�zA�v�A�v+A�t�A�p;A�p�A�XyA�Q�A�?}A�MAˢ�A�8�A��Aɦ�A��AȨ$A�]/A�<�A�I�A�I�A�9�A��A��A��VA���A��A���AǓA�.Aƿ�A�H�AĐbA��`A�خA�&A���A�� A�}�A�~�A�tTA� �A�5?A��WA��%A�YA��CA��A�~�A�3�A�PA�A���A��0A���A���A�R�A��A��UA�FA�JA�`vA�!bA�E�A�W�A���A�.�A��TA�}�A�n�A�(�A�� A�NA��A���A�n�A��wA~�HA{�IAw�BAr,=Ak|Af�A^��A[`BAZ=�AX�#AU�+AR��AO�dAN�ANG�AMjALB�AC�@A?c�A>�A<O�A;��A;+�A9U2A8CA7�A4 iA1��A0%A/�PA.�A.bNA+��A)�|A)8�A(��A'��A'!A&�A#��A"�A!�DA�oA��AQA�2A�KAFtAZAo�A��AsAb�AVmA�Ap�A�;A�A"hA͟A��A�4AE9AMA��A�BAz�AAخA�OA�VA��A$tAZA�9A��Ah
ASAxA�A��A��A��A��A��A��A
��A	��AbNA�A��AA��A�A>BA��AE�AAߤA�<A�nA�IA��AH�A ϫA u�A @�@�4�@���@��}@�rG@� i@�ȴ@���@�˒@��@�oi@���@��@�^�@�tT@��E@�@��@�j@�,=@��A@�1'@�:*@��@�m]@��@�ƨ@�j@��@�j@�S�@�*�@��@��N@�X@�x@�^�@�M@��@��B@�E�@��@��c@�+k@�;d@�l�@��;@�4@�u�@�dZ@�0�@��f@��u@�s�@��T@�L�@��@���@��?@ۗ$@��@��)@ڶ�@ڙ1@�z@�R�@�.�@��@��
@�C@��@��@���@��@ֽ<@�s�@�(�@��@���@�U�@ԭ�@ԉ�@�i�@�C�@���@Ӛk@�g8@ў�@��@У�@ЋD@�s�@ϓ@��2@�K^@�4@�1@�	@��.@�N<@�C�@˥�@�T�@�=@��@���@�z�@�7@ɡ�@�[W@���@ș1@�Z@�M@�c�@���@�s@���@�7�@��@�8@¹�@�l�@���@��F@��@��w@���@�(�@���@�E�@���@��~@�e�@�O@���@���@�>B@��g@�|@�+@��,@���@�~�@�4@���@�ϫ@��0@�S�@�2�@��z@�zx@�V@��L@��@���@��@��@�b�@�(�@���@�i�@�@��&@���@���@��Q@��{@��@���@��b@�Z�@��a@���@���@�e�@��M@�M@��~@�=�@�+@���@��u@���@��n@�H�@���@���@�{�@�kQ@�K^@�_@��w@�iD@�e�@�P�@��@�
=@��f@���@�c @��@��@���@��@�(�@���@��n@���@���@�|@�x@�m]@�[W@�T�@�O�@�G�@�>�@�
=@��9@���@��+@�l�@�H�@�%�@�@�G@��)@�ԕ@�j�@��1@�n�@�I�@�+k@��@�u@���@��)@�y�@��@��x@�W�@���@�\�@�N<@��c@��4@�U2@��@���@��Q@��0@���@��7@�\)@�#�@���@�r�@�O@��6@��@�2a@��@��E@��!@�u%@�C-@�&�@�x@���@���@��M@�=@���@��`@���@���@��@��@���@��@�ѷ@��$@���@�|�@�u%@��@��S@�\�@���@���@�`�@�8�@��@���@�ϫ@�~�@�@O@��@���@�oi@��@���@�@O@��@��"@��@���@���@��@�hs@� \@��I@�U2@�.�@��@���@�`B@��@���@�j@�)�@�e@��@�	�@��@��@�rG@��@�@��@�tT@�Ov@�;�@�@���@�4�@��@��)@�bN@� �@�خ@���@�A @��|@���@��N@�:�@��@���@�_@�o�@�q@��`@�֡@���@�y>@�GE@j�@~�b@~-@}��@}��@}	l@|�@|�D@{�q@{E9@zxl@zL0@z.�@y��@y\�@x��@x�@x%�@w�a@w��@wiD@wZ�@wS�@wRT@w�@vh
@v#:@u��@uO�@t�@t|�@t%�@sݘ@s��@s!-@r͟@r�6@r��@r �@qm]@q \@p�`@pQ�@ol�@o�@n�@n@�@m��@m�@l"h@j�@i��@i/@hی@h��@h��@h2�@gU�@f��@f.�@e�9@e�'@e�h@eo @eX@e5�@e�@d֡@d�@d*�@c�@c]�@b��@b�@b�m@b��@b3�@a�T@ax�@`�[@`Xy@_�r@_��@_9�@^�@^u%@]�D@]�@]�M@]Q�@]A @]	l@\U2@\G@[�K@[x@[U�@Z�<@Z�@Yw2@X��@X�@X1@W��@Wo�@V��@Vz@V3�@U��@Uhs@U!�@T��@T$@SC@R�L@RYK@RC�@R5?@R$�@Q�.@Q�@Q��@Q��@Q��@Q�M@Q=�@P�f@P�_@O��@O�P@O��@Ot�@OO@O;d@N�m@N3�@M�'@MVm@L�@Ly>@L6@L'R@L~@LM@K�@KE9@K$t@K
=@J{�@J �@I�"@I&�@H�v@H��@H,=@G��@G��@G��@G\)@GE9@G1�@G@F�"@F�s@F��@FW�@E�^@Eu�@E[W@EL�@E	l@D�U@Dw�@Dh�@D>B@DG@C�A@Cخ@C��@C$t@B�R@B��@Bu%@BE�@A�N@A|@A=�@@��@@u�@?�w@?l�@>��@>Z�@>�@=p�@<��@<D�@;{J@;�@:�,@:-@9�>@9��@9��@98�@9�@8�@8�@7�
@7��@7�@6E�@5(�@4'R@3��@2��@2c @2�@1��@1��@0�v@0|�@0r�@0q@0oi@0g8@0D�@0�@/�
@/��@/�@.�r@.c @-�@-��@-^�@-[W@-T�@-/@-!�@-q@-�@,�/@,~(@,�@+�[@+U�@+�@*�@*n�@*4@)��@)�7@)4@(�5@(��@(h�@(@'�K@'��@'�k@'�4@'a@'O@'8@'&@&�+@&8�@&{@& �@%ԕ@%��@%c�@%V@$��@$�@$��@$~(@$4n@$�@#�r@#��@#��@#s@#n/@#l�@#g�@#@O@#Y@"��@"�@"�}@"}V@"J�@"4@!��@!��@!��@!rG@!Dg@!0�@!0�@!;@ ��@ ��@ oi@ PH@ �@��@��@��@/�@��@ߤ@��@�@n�@8�@&�@�@
�@�Z@�=@\�@2a@�@�5@�K@��@�I@y>@g8@A�@��@x@/�@�@Y@ i@��@��@�<@xl@=q@($@	@�T@hs@X@Y�@7L@ѷ@�D@%�@1@�@��@�0@��@��@�@��@��@�k@��@��@�$@��@��@�	@v`@>�@�@�B@�b@v�@Q@=q@�9@��@��@��@u�@j@<6@+@�@�	@��@�D@tT@2�@��@��@�	@t�@e�@RT@@O@/�@
=@ȴ@h
@Ov@;�@O@��@�@��@T�@@@|�@ �@�@�f@��@��@��@C�@�D@Q�@(�@�P@��@z�@��@�@�@@g�@C�@9�@4�@,�@�@
��@
��@
�6@
�@
�A@
~�@
_�@	�.@	��@	�z@	��@	��@	�3@	�@	��@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�`�A�c A�e`A�o5A�qAA�r�A�s�A�s�A�tTA�s�A�tTA�tA�u�A�x8A�y�A�zxA�zA�v�A�v+A�t�A�p;A�p�A�XyA�Q�A�?}A�MAˢ�A�8�A��Aɦ�A��AȨ$A�]/A�<�A�I�A�I�A�9�A��A��A��VA���A��A���AǓA�.Aƿ�A�H�AĐbA��`A�خA�&A���A�� A�}�A�~�A�tTA� �A�5?A��WA��%A�YA��CA��A�~�A�3�A�PA�A���A��0A���A���A�R�A��A��UA�FA�JA�`vA�!bA�E�A�W�A���A�.�A��TA�}�A�n�A�(�A�� A�NA��A���A�n�A��wA~�HA{�IAw�BAr,=Ak|Af�A^��A[`BAZ=�AX�#AU�+AR��AO�dAN�ANG�AMjALB�AC�@A?c�A>�A<O�A;��A;+�A9U2A8CA7�A4 iA1��A0%A/�PA.�A.bNA+��A)�|A)8�A(��A'��A'!A&�A#��A"�A!�DA�oA��AQA�2A�KAFtAZAo�A��AsAb�AVmA�Ap�A�;A�A"hA͟A��A�4AE9AMA��A�BAz�AAخA�OA�VA��A$tAZA�9A��Ah
ASAxA�A��A��A��A��A��A��A
��A	��AbNA�A��AA��A�A>BA��AE�AAߤA�<A�nA�IA��AH�A ϫA u�A @�@�4�@���@��}@�rG@� i@�ȴ@���@�˒@��@�oi@���@��@�^�@�tT@��E@�@��@�j@�,=@��A@�1'@�:*@��@�m]@��@�ƨ@�j@��@�j@�S�@�*�@��@��N@�X@�x@�^�@�M@��@��B@�E�@��@��c@�+k@�;d@�l�@��;@�4@�u�@�dZ@�0�@��f@��u@�s�@��T@�L�@��@���@��?@ۗ$@��@��)@ڶ�@ڙ1@�z@�R�@�.�@��@��
@�C@��@��@���@��@ֽ<@�s�@�(�@��@���@�U�@ԭ�@ԉ�@�i�@�C�@���@Ӛk@�g8@ў�@��@У�@ЋD@�s�@ϓ@��2@�K^@�4@�1@�	@��.@�N<@�C�@˥�@�T�@�=@��@���@�z�@�7@ɡ�@�[W@���@ș1@�Z@�M@�c�@���@�s@���@�7�@��@�8@¹�@�l�@���@��F@��@��w@���@�(�@���@�E�@���@��~@�e�@�O@���@���@�>B@��g@�|@�+@��,@���@�~�@�4@���@�ϫ@��0@�S�@�2�@��z@�zx@�V@��L@��@���@��@��@�b�@�(�@���@�i�@�@��&@���@���@��Q@��{@��@���@��b@�Z�@��a@���@���@�e�@��M@�M@��~@�=�@�+@���@��u@���@��n@�H�@���@���@�{�@�kQ@�K^@�_@��w@�iD@�e�@�P�@��@�
=@��f@���@�c @��@��@���@��@�(�@���@��n@���@���@�|@�x@�m]@�[W@�T�@�O�@�G�@�>�@�
=@��9@���@��+@�l�@�H�@�%�@�@�G@��)@�ԕ@�j�@��1@�n�@�I�@�+k@��@�u@���@��)@�y�@��@��x@�W�@���@�\�@�N<@��c@��4@�U2@��@���@��Q@��0@���@��7@�\)@�#�@���@�r�@�O@��6@��@�2a@��@��E@��!@�u%@�C-@�&�@�x@���@���@��M@�=@���@��`@���@���@��@��@���@��@�ѷ@��$@���@�|�@�u%@��@��S@�\�@���@���@�`�@�8�@��@���@�ϫ@�~�@�@O@��@���@�oi@��@���@�@O@��@��"@��@���@���@��@�hs@� \@��I@�U2@�.�@��@���@�`B@��@���@�j@�)�@�e@��@�	�@��@��@�rG@��@�@��@�tT@�Ov@�;�@�@���@�4�@��@��)@�bN@� �@�خ@���@�A @��|@���@��N@�:�@��@���@�_@�o�@�q@��`@�֡@���@�y>@�GE@j�@~�b@~-@}��@}��@}	l@|�@|�D@{�q@{E9@zxl@zL0@z.�@y��@y\�@x��@x�@x%�@w�a@w��@wiD@wZ�@wS�@wRT@w�@vh
@v#:@u��@uO�@t�@t|�@t%�@sݘ@s��@s!-@r͟@r�6@r��@r �@qm]@q \@p�`@pQ�@ol�@o�@n�@n@�@m��@m�@l"h@j�@i��@i/@hی@h��@h��@h2�@gU�@f��@f.�@e�9@e�'@e�h@eo @eX@e5�@e�@d֡@d�@d*�@c�@c]�@b��@b�@b�m@b��@b3�@a�T@ax�@`�[@`Xy@_�r@_��@_9�@^�@^u%@]�D@]�@]�M@]Q�@]A @]	l@\U2@\G@[�K@[x@[U�@Z�<@Z�@Yw2@X��@X�@X1@W��@Wo�@V��@Vz@V3�@U��@Uhs@U!�@T��@T$@SC@R�L@RYK@RC�@R5?@R$�@Q�.@Q�@Q��@Q��@Q��@Q�M@Q=�@P�f@P�_@O��@O�P@O��@Ot�@OO@O;d@N�m@N3�@M�'@MVm@L�@Ly>@L6@L'R@L~@LM@K�@KE9@K$t@K
=@J{�@J �@I�"@I&�@H�v@H��@H,=@G��@G��@G��@G\)@GE9@G1�@G@F�"@F�s@F��@FW�@E�^@Eu�@E[W@EL�@E	l@D�U@Dw�@Dh�@D>B@DG@C�A@Cخ@C��@C$t@B�R@B��@Bu%@BE�@A�N@A|@A=�@@��@@u�@?�w@?l�@>��@>Z�@>�@=p�@<��@<D�@;{J@;�@:�,@:-@9�>@9��@9��@98�@9�@8�@8�@7�
@7��@7�@6E�@5(�@4'R@3��@2��@2c @2�@1��@1��@0�v@0|�@0r�@0q@0oi@0g8@0D�@0�@/�
@/��@/�@.�r@.c @-�@-��@-^�@-[W@-T�@-/@-!�@-q@-�@,�/@,~(@,�@+�[@+U�@+�@*�@*n�@*4@)��@)�7@)4@(�5@(��@(h�@(@'�K@'��@'�k@'�4@'a@'O@'8@'&@&�+@&8�@&{@& �@%ԕ@%��@%c�@%V@$��@$�@$��@$~(@$4n@$�@#�r@#��@#��@#s@#n/@#l�@#g�@#@O@#Y@"��@"�@"�}@"}V@"J�@"4@!��@!��@!��@!rG@!Dg@!0�@!0�@!;@ ��@ ��@ oi@ PH@ �@��@��@��@/�@��@ߤ@��@�@n�@8�@&�@�@
�@�Z@�=@\�@2a@�@�5@�K@��@�I@y>@g8@A�@��@x@/�@�@Y@ i@��@��@�<@xl@=q@($@	@�T@hs@X@Y�@7L@ѷ@�D@%�@1@�@��@�0@��@��@�@��@��@�k@��@��@�$@��@��@�	@v`@>�@�@�B@�b@v�@Q@=q@�9@��@��@��@u�@j@<6@+@�@�	@��@�D@tT@2�@��@��@�	@t�@e�@RT@@O@/�@
=@ȴ@h
@Ov@;�@O@��@�@��@T�@@@|�@ �@�@�f@��@��@��@C�@�D@Q�@(�@�P@��@z�@��@�@�@@g�@C�@9�@4�@,�@�@
��@
��@
�6@
�@
�A@
~�@
_�@	�.@	��@	�z@	��@	��@	�3@	�@	��@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�bB��B�B�B�B�B��B�NB�B��B�B�B�B�`B�2B�XB��B�B�kB�QB��B��B	�B	aB	�,B	��B	�aB
�|B
�B�BPB�B�BMB�B�B�B[B�B&B�B?B!�B+kB6B`�B��B�;B�qB�B��BqB �B!B#TB1�B3B2�B'�B!�BVBWB_BB�B��B� B�=B�B��B��B�)B��B��BxBeBGB-]B?B�B
��B
�HB
�+B
�B
}�B
T�B
=VB
7B
4B
)DB
�B	�B	�NB	��B	�YB	Z�B	/�B	)B	�B	�B	UB�B�B��B�&B��B�kBܒB�aB�oB��B��B�B��B	B	�B	mB	MB	�B	�B	�B	�B	9B	�B	�B	5B	 \B	�B	 �B	!�B	pB	;B	%�B	/OB	D�B	WsB	s�B	��B	��B	�#B	�tB	�B	��B	�5B	��B	�DB	��B	�YB	ɠB	�<B	�\B	��B	ϫB	�vB	�B	��B	�pB	͟B	�dB	�DB	ɠB	��B	��B	��B	��B	�vB	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	�B	ĶB	�[B	��B	��B	��B	ňB	�B	�KB	��B	�RB	��B	�=B	�	B	��B	�^B	̘B	�6B	�PB	�B	�aB	֡B	�1B	�B	�QB	چB	�CB	ݘB	�B	��B	�B	�@B	�_B	�*B	�B	��B	�B	�B	�$B	�B	�cB	��B	� B	�IB	�)B	��B	�B	�B	�B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�)B	��B	�]B	��B	�"B	��B	�]B	��B	�IB	�B	�B	��B	��B	��B	�B	�B	� B	��B	�)B	��B	�B	�B	�OB	�B	�B	�B	�9B	�9B	�B	�B	�aB	�|B	�B	�B	�B	�B	�B	��B	�IB	�B	��B	�B	�B	�[B	�[B	�vB	�vB	�vB	�'B	�AB	�|B	��B	�B	�%B	�?B	��B	�B	�B	�nB	�nB	��B	��B	��B	��B	�+B	�B	��B	�$B	�0B	�0B	�6B	��B	��B	�B	��B	�"B	�B	�"B	�VB	�<B	��B	��B	�(B	�(B	�(B	�wB	��B	��B	�HB	��B
 B
 iB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
'B
�B
�B
�B
GB
�B
B
mB
�B
�B
B
?B
tB
YB
YB
�B
mB
mB
�B
�B
�B
YB
�B
�B
_B
�B
1B
	7B

rB

�B

�B
�B
^B
�B
JB
�B
B
�B
�B
�B
�B
"B
�B
(B
B
(B
vB
�B
�B
.B
B
NB
4B
NB
�B
:B
aB
�B
�B
�B
�B
�B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
	B
�B
�B
WB
�B
B
)B
)B
CB
xB
CB
�B
�B
�B
B
dB
IB
B
B
�B
�B
B
5B
�B
�B
�B
�B
�B
�B
VB
�B
�B
�B
�B
 BB
"B
!�B
!�B
!�B
!�B
!�B
!�B
!|B
"NB
"�B
"�B
#:B
#�B
#�B
#�B
$B
#�B
$B
$ZB
$�B
$�B
$�B
%FB
%�B
&B
&fB
&�B
&�B
&�B
&�B
&�B
'RB
(sB
(�B
*eB
*�B
*�B
*�B
*�B
+�B
,WB
,�B
,�B
-)B
-)B
-CB
-CB
-CB
-�B
-�B
.IB
.IB
.}B
.cB
.}B
.�B
.}B
.�B
/OB
/�B
/�B
0;B
0UB
0�B
0oB
1B
0�B
1[B
2GB
2�B
3B
3B
3�B
4�B
4�B
5B
5B
5?B
5%B
5%B
5�B
6zB
6�B
6�B
6�B
6�B
6�B
7B
7�B
7�B
8lB
8RB
88B
8�B
8�B
9	B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:*B
:xB
:�B
:�B
;B
;0B
;JB
;0B
<B
;�B
;�B
;�B
<PB
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>BB
>wB
>�B
?HB
@4B
@�B
A;B
A�B
AoB
AoB
A�B
B'B
B[B
B�B
B�B
CB
B�B
CB
CB
C-B
C-B
C-B
C{B
C�B
DB
D�B
D�B
D�B
D�B
EB
EB
ESB
E�B
F%B
F�B
F�B
G+B
GzB
G�B
G�B
H1B
HB
HKB
HKB
H1B
HKB
H�B
IB
I7B
IlB
IlB
J#B
JXB
J�B
KB
KB
KxB
K�B
KxB
K�B
K�B
K�B
LJB
L�B
L�B
L�B
M6B
NB
N"B
NVB
N<B
NVB
N<B
NpB
NVB
NpB
NpB
NVB
N�B
N�B
N�B
OB
P.B
P.B
PB
PB
P.B
P.B
P�B
Q4B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
SuB
SuB
S�B
T,B
TaB
T�B
T�B
T�B
U2B
UMB
U�B
U�B
U�B
VB
VB
VB
VB
VSB
VSB
V�B
WYB
WsB
W�B
WsB
W�B
W�B
W�B
XB
X+B
XEB
XEB
XEB
X_B
X�B
Y1B
Y1B
Y1B
YKB
Y�B
Y�B
ZB
ZB
Z�B
[=B
[=B
[�B
[�B
[�B
\xB
\]B
\�B
]�B
^5B
^jB
_�B
_�B
_�B
_�B
`'B
`'B
`BB
`BB
`�B
`BB
`�B
a-B
a�B
b�B
b�B
c�B
c�B
d@B
d@B
d�B
eFB
e`B
e`B
e`B
e`B
e`B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hsB
h�B
h�B
iDB
i_B
i�B
i�B
jKB
j�B
j�B
kB
kB
kQB
k�B
k�B
lB
l"B
l"B
lWB
lqB
lqB
l�B
lqB
m)B
m]B
m]B
m]B
m�B
m�B
m�B
n/B
n/B
nIB
n/B
n�B
n�B
n�B
n�B
oB
oOB
oOB
oOB
oOB
oOB
o�B
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
p�B
p�B
p�B
qB
p�B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
rB
q�B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s3B
shB
shB
shB
s�B
tB
tB
tB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
vzB
vzB
vzB
v�B
wLB
wLB
wB
wfB
w�B
w�B
xlB
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y>B
y�B
y�B
y�B
y�B
zB
y�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
z�B
z�B
{dB
{dB
{B
{�B
{�B
|B
|6B
|PB
|PB
|jB
|jB
|jB
|jB
|�B
}B
}"B
}"B
}<B
}<B
}<B
}�B
}�B
}�B
~�B
~�B
~�B
HB
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
�B
��B
�GB
�B
�{B
��B
�{B
��B
��B
��B
��B
�B
�B
�3B
�3B
�3B
�MB
��B
�B
�B
�B
�B
�B
�B
�B
�B
�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�bB�B�4B�-B�-B��B�B�hB��B�B��B�&B�,B�zB�LB�sB��B�B�B�B��B�B	�B	?B	��B	�B	��B
��B
ޞB3B�B�B�BgB�B,BBuB�BuB�B_B#B-]B:^Bf�B��BĜB�jB�B  B�B"B&LB*KB5%B7�B8B*�B#nB vB�BWB�B	�B �B�2B�B�%B�UB�B�AB�NB�PB{�Bi_BK�B1[B	BYB
�B
�B
�xB
��B
�B
W�B
>]B
8B
6FB
,�B
B
�B	��B	��B	��B	b4B	3�B	B	B	�B	B��B��B��B�2B��B�B�B�SB�{B��B�@B��B�xB	B	NB	1B	�B	�B	�B	TB	B	+B	�B	�B	VB	!�B	!�B	#:B	#�B	!|B	!�B	'B	0�B	D�B	W�B	sB	��B	��B	�#B	��B	��B	�B	��B	��B	�0B	�(B	��B	�	B	�pB	ϑB	�B	��B	ϫB	�\B	�\B	��B	�B	��B	��B	˒B	ɺB	�<B	��B	�B	�GB	�vB	��B	��B	�3B	�B	��B	�dB	�.B	�B	�2B	�.B	�mB	��B	�B	�aB	��B	��B	��B	�?B	ǮB	ȚB	�B	ɆB	�#B	�XB	�=B	�XB	��B	�B	͹B	�<B	��B	�2B	�sB	�B	�kB	ڠB	�=B	��B	�B	�pB	��B	�|B	�B	�KB	��B	�$B	�RB	�B	�B	�$B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�RB	�RB	�B	�eB	�B	�B	�eB	�DB	�KB	�kB	�qB	�IB	�/B	�CB	�B	�)B	��B	�/B	�B	�wB	�]B	�IB	��B	�B	�)B	�B	�B	� B	�oB	�!B	�B	�B	�B	��B	�B	��B	�B	�B	�)B	�B	�B	�iB	��B	�hB	�9B	�B	�nB	�B	�B	�|B	�B	��B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�?B	�ZB	�tB	�?B	��B	�nB	��B	��B	�B	�B	�B	�tB	�B	�lB	�>B	��B	�B	��B	��B	�6B	�jB	��B	�<B	�VB	�<B	�qB	��B	��B	��B	�(B	�BB	�]B	�wB	��B	��B	�.B	��B
  B
 iB
 �B
 �B
B
 B
 �B
 �B
B
UB
AB
uB
�B
�B
�B
{B
B
9B
�B
�B
B
YB
tB
�B
�B
B
EB
�B
�B
B
%B
?B
�B
�B
�B
�B
B
�B
	�B

�B

�B
)B
�B
�B
�B
�B
�B
PB
�B
B
�B
<B
pB
�B
\B
(B
BB
�B
�B
�B
}B
NB
hB
hB
�B
 B
�B
�B
�B
�B
�B
�B
B
2B
B
2B
2B
MB
gB
�B
?B
�B
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
B
1B
B
B
1B
QB
7B
B
B
�B
#B
#B
�B
�B
B
)B
]B
CB
xB
�B
xB
/B
/B
/B
IB
�B
�B
dB
OB
B
B
5B
OB
�B
�B
�B
�B
B
!B
�B
�B
�B
�B
 'B
 �B
"4B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
# B
#B
#nB
#�B
$B
$B
$&B
$&B
$@B
$�B
$�B
$�B
%B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
'B
&�B
'�B
(�B
)DB
*�B
*�B
*�B
*�B
+B
,B
,�B
-B
-)B
-CB
-CB
-]B
-]B
-�B
-�B
./B
.cB
.}B
.�B
.�B
.�B
.�B
.�B
/ B
/�B
/�B
0B
0�B
0�B
0�B
0�B
1[B
1[B
1�B
2�B
2�B
3hB
3�B
4TB
4�B
4�B
5?B
5?B
5tB
5ZB
5�B
6B
6�B
6�B
6�B
6�B
7B
7B
7fB
8B
8B
8�B
8lB
8lB
8�B
9$B
9$B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:DB
:xB
:�B
:�B
:�B
;JB
;JB
;dB
;dB
<6B
<B
<B
<B
<�B
<�B
<�B
<�B
=<B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
AoB
A�B
A�B
A�B
BB
B[B
B�B
B�B
B�B
CB
C-B
C-B
C-B
CGB
CGB
CGB
C�B
C�B
D3B
D�B
D�B
D�B
D�B
EB
ESB
E�B
E�B
FYB
F�B
GB
GzB
G�B
G�B
HB
HKB
H1B
HfB
H�B
HKB
H�B
IB
IB
IlB
I�B
I�B
JXB
J�B
J�B
K)B
KDB
K�B
K�B
K�B
K�B
LB
L0B
L�B
L�B
L�B
MB
M�B
N<B
NVB
NVB
NVB
NVB
NpB
N�B
NpB
NpB
NpB
NpB
N�B
N�B
OB
OvB
PHB
P.B
P.B
P.B
PHB
PbB
P�B
QhB
Q�B
R:B
R�B
R�B
SB
R�B
SB
SB
SB
SuB
S�B
S�B
T,B
TaB
T�B
T�B
T�B
UMB
UMB
UgB
U�B
U�B
VB
VB
VB
V9B
V9B
VmB
V�B
V�B
WsB
W�B
W�B
W�B
W�B
XB
X+B
X+B
XEB
X_B
XyB
XyB
X�B
YB
YKB
YKB
YKB
Y�B
ZB
ZB
ZB
ZQB
Z�B
[qB
[qB
[�B
\CB
\)B
\�B
\�B
]/B
]�B
^OB
^�B
_�B
_�B
_�B
`B
`\B
`BB
`�B
`vB
`�B
`vB
`�B
a�B
bB
b�B
c:B
c�B
dB
dZB
d�B
d�B
ezB
e`B
e`B
ezB
e`B
ezB
e�B
e�B
e�B
fB
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h
B
h
B
h>B
h�B
h�B
iB
i_B
iyB
jB
jB
jeB
j�B
j�B
kB
kQB
kkB
k�B
lB
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
mCB
mwB
mwB
m�B
m�B
m�B
m�B
nIB
nIB
ncB
ncB
n�B
n�B
o B
o B
oOB
oiB
oOB
oOB
oOB
oiB
o�B
o�B
p!B
pB
pB
p!B
pUB
poB
p�B
p�B
p�B
p�B
qB
qB
qB
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
rB
r�B
r�B
r�B
r�B
r�B
s3B
shB
s3B
shB
s�B
s�B
s�B
tB
t9B
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
vzB
v�B
v�B
v�B
wfB
wLB
w2B
w�B
w�B
xB
xlB
x�B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
x�B
x�B
x�B
x�B
y$B
y$B
yXB
y�B
y�B
y�B
z*B
z*B
zB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{�B
{B
{�B
{�B
|B
|B
|6B
|PB
|jB
|jB
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}VB
}VB
}VB
}�B
}�B
~B
~�B
B
.B
}B
�B
�iB
��B
��B
� B
��B
��B
��B
�B
�AB
�B
�GB
�-B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�gB
�3B
�MB
�gB
�B
�B
�B
�9B
�B
�B
�B
�B
�B
�S33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904030054462019040300544620190403005446202207271130282022072711302820220727113028202207271533152022072715331520220727153315  JA  ARFMdecpA30a                                                                20190523095840  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100021  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100022  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100022  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100022  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100023                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111514                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190324000000  CF  PSAL_ADJUSTED_QC@=q@�  G�O�                JM  ARCAJMQC2.0                                                                 20190402155446  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190402155446  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023028  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063315  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                