CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-21T09:37:49Z creation;2019-08-21T09:37:51Z conversion to V3.1;2022-08-02T05:12:12Z update;     
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
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190821093749  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_020                    2C  D   APEX                            8420                            2.11.2                          846 @��aZ��1   @��a�g(�@-����>�c�C�\��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A!��AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BZffB`  Bi33Bn��Bw��B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  BB���B�  B�  C   C  C  C  C  C
33C  C33C�3C�fC  C  C  C  C  C  C   C"  C$  C&� C'�3C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��
@�33Ap�A"{ABffAb=qA���A��RA���A���A��\AиRA���A��\B 33B=qB(�B33B (�B((�B0G�B8\)B@\)BH\)BP\)BZ�B`=qBiz�Bo=qBw�B�\B�B�(�B�ǮB���B��B��B�{B��B��B��B��B���B��B��HB��B�#�B�#�B��B�(�B�{B��B�.B�=qB�L�B�Q�B�
=B�=qB��
B��B��B�#�C �C
C�C{C�C
B�C
=CJ=C��C�C�C\C�C
=C�C{C �C"
C$)C&�{C'�C*�C,�C.�C0{C2�C4�C6\C8\C:{C<
C>{C@�CB�CD�CF�CH
CJ{CL
CN{CP
CR
CT�CV{CX�CZ�C\{C^)C`)Cb
Cd�Cf�Ch
=Cj\Cl�Cn!HCp
Cr\Ct\Cv\Cx
Cz
C|�C~
C��C�
=C��C�
=C�
=C��C�C�C��C��C�
=C��C�C��C�fC��C��C��C�fC��C��C��C�
=C�C�
=C�
=C�
=C��C��C��C��C�
=C��C��C�C��C��C��C��C��C��C��C��C��C��C��C�
=C�
=C�
=C��C�fC��C��C�C�fC��C��C�\C�
=C��C�
=C��C��C��C�fC��C��C�fC��C��C��C��C��C��C��C��C��C��C�fC�fC�
=C��C��C�
=C��C��C��C��C�fC��C��C��C��C��C��C��C��C��C�\C�C�C�C��C��C��C�\C��C��C�C�C�
=C��C�\C�
=C��C��C��C��C��C�
=C��C��C��C��C��C��C�fC��D �D ��D{D�3DD�{D{D�fDD��D�D��D�D�{D�D��D{D�fD	
D	��D
{D
��D�D�fDfD��DD�{D�D��DfD��D�D�{D�D�
DD��D�D�{D3D�D{D�3D�D�{D�D��D�D�3D�D�D�D�{D�D�{DfD��D3D��D{D�{D�D�D �D �
D!fD!�fD"fD"�D#�D#�D$3D$�D%D%��D&{D&�{D'�D'�D({D(�3D)�D)�{D*
D*��D+D+�{D,�D,�fD-D-��D.�D.��D/{D/�D0�D0��D1D1�D2�D2��D3D3�
D4fD4�fD5D5�D6fD6�D7�D7�{D8�D8��D9�D9��D:D:��D;3D;��D<3D<��D=�D=�{D>
D>��D?RD?�fD@�D@�DADA�fDBRDB�fDCDC�DD�DD�fDEDE�{DFDF�DG�DG�{DH3DH�3DI{DI�{DJDJ�
DK
DK�{DL�DL��DM�DM�DN{DN��DO�DO�DPfDP��DQ�DQ�{DRDR��DS3DS�DTfDT�fDU�DU�{DV�DV��DW�DW��DX{DX��DY�DY��DZDZ��D[{D[��D\{D\��D]�D]�D^D^�{D_{D_�D`{D`��DafDa�fDb{Db�fDcRDc�
Dd
Dd�{DeDe��Df�Df��Dg{Dg�{DhDh�fDiDi�{Dj�Dj��Dk{Dk��Dl{Dl��DmfDm��Dn3Dn��DoDo�{Dp�Dp��Dq{Dq�DrDr�fDs�Ds��DtHDt�{Du�Du��Dv�Dv�Dw{Dw�
Dx
Dx��DyRDy�fDz{Dz�D{
D{�fD|�D|�
D}D}�3D~�D~�D{D�fD�3D�B=D���D�ÅD��D�B�D���D��=D�=D�B�D���D�ÅD�3D�B�D���D�D��D�A�D���D�ÅD��D�B�D���D���D�3D�B�D���D�D�=D�B=D���D���D�=D�B�D���D���D�=D�B�D��=D���D�=D�C�D���D�ÅD��D�B�D���D�D��D�B�D���D��3D��D�A�D��=D�D�=D�A�D���D���D�=D�B=D��=D��)D��D�AHD��=D�D��D�C�D��3D��3D��D�B=D���D���D�=D�A�D���D��=D��D�C�D���D���D�3D�B�D���D���D��D�D�D��3D���D��D�B=D���D�D��D�B=D���D���D��D�B=D���D�D��D�C�D��3D���D��D�B�D��3D���D��D�B�D���D��=D�=D�A�D��3D�ÅD��D�C3D���D���D��D�B�D���D�D�=D�C�D���D��=D��D�C3D���D��=D��D�B�D���D��=D�=D�B�D���D��=D��D�A�D���D���D��D�B�D��)D��3D�=D�B�D���D���D�=D�B=D���D���D��D�B=D���D��HD��D�B�D��=D���D��D�B=D���D���D��D�B=D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D�D��D�B�D���D��3D��D�B�D��3D���D��D�B=D��=D���D�=D�A�D��=D���D��D�B�D���D��3D�3D�B=D��=D���D��D�B�D���D���D�=D�B=D���D��=D��D�C3D��3D���D�HD�B=D���D��=D��D�C3D��=D���D�HD�B�D���D���D��D�@�D���D��3D�3D�B�D��=D���D�=D�C3D���D�ÅD��D�A�D���D���D��D�A�D��=D�D��D�B�D���D�D��D�A�D���D�ÅD��D�A�D=D�D��D�C3DÃ3D���D� �D�A�DĂ�D��=D��D�B=DŃ3D���D��D�C3DƂ=D��=D��D�C3Dǂ�D�D�=D�B�DȂ�D��3D��D�A�DɁ�D���D��D�A�Dʂ�D��3D��D�B=D˃3D��)D��D�C3D̃3D��3D�3D�B�D͂�D�ÅD��D�C�D΃3D��=D�=D�B=Dς�D���D��D�B�DЂ�D�D��D�B�Dт=D���D��D�B�D҃3D�D��D�B=Dӂ�D�D��D�B=DԂ=D�D��D�B�DՁHD���D�=D�AHDց�D���D��D�B�D׃�D��3D��D�B�D؁�D���D��D�A�Dـ�D��=D��D�B=Dڂ=D�D��D�C�Dۂ�D�D��D�A�D܁�D���D��D�B�D݃3D�ÅD��D�C�Dނ=D���D��D�A�D߂�D���D�=D�B�D���D�D��D�B�D��D��=D��D�B�D₏D�D��D�B�D��D���D��D�C3D䃅D��3D��D�A�D�3D���D��D�B�D��D��=D��D�B=D�3D��3D�3D�C�D肏D�D�3D�C3D�3D���D��D�B=D�HD���D�=D�C3D��D���D��D�A�D��D�ÅD��D�B�D��D��HD��D�B�D��D���D��D�C�D�=D��HD�=D�A�D���D���D��D�A�D�D��3D��D�B�D��D�ÅD�)D�C�D��D�D�=D�C3D�D�D��D�@�D��=D���D�3D�B�D���D��3D��D�C�D���D���D��D�B�D��3D��=D�HD�B=D���D���D��D�B�D���D��3D��D�A�D���D��{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�:�A�?�A�9�A�=qA�K�A�N�A�N�A�PA�P�A�Q�A�Q�A�N�A�P}A�LdA�L0A�<�A��zA�I�A�A��lA�;0A�~�AݭA��(A�/�A�P�A� 4A��Aـ�A���A��%A�0UAѽ<AΦA̔{A˳hAǜxA��A�#�A�ɺA�4�A��/A�[WA��3A���A�<jA��A���A�4�A���A��A���A�w�A�� A�0!A���A��A��?A��A��A��aA�.IA�=A��TA���A���A���A�ʌA��A���A�6�A�;A���A�u�A��A�QNA��vA�ÖA���A�A|&�Aw�jAtHAoxlAkߤAhA Af��Ab_�A\�HAZ�AX�AU�AQ�mAM�XAL��AKAJ$tAI�AG�AFZ�ACA?~A<�A::*A7�A66A44A2U�A0��A/�A/(A.B�A-�UA-��A,��A,�1A,!�A*�oA)A&�sA&DgA&+�A&;A%Z�A#ɆA"��A!��A �A �!A �A qvA K^A!��A"�{A"
=A!@OA kQA��A�A�AA�AsA��A��A�A�^A�:A�DA�?A�VA��AW�A��A�A�.A1'A��A>BA�>AC�A	A6Av`A�LA*�AԕA{JAjA��A��A��AϫA�A�"A7A�'Aj�A�KA�AA�A
��A
��A
8�A	��A	c�A��At�A�KA
�A
�A	=A	O�A	(A{�AC-A�KA��AtTA4Al�A&A��A_�A1�AGA��A8A�A�A!�A �A w2A Y@���@���@��@�C�@���@�O@���@��@�;�@��@��@�e,@��@�-�@�|�@���@�R�@�G@�_@��@�k�@�~@� \@��@�o�@점@��#@�e,@�;d@� \@���@��@쒣@�kQ@�Ov@��@�]�@�@�'@�Dg@�F@�@��@�Dg@�q@��y@�j@�%�@�a�@�0U@���@��@�J�@�M@��@��@��@�H@��9@��6@�a@���@�c@�c @�x@���@�P�@�
=@��U@�z�@��D@�rG@��@��`@ج@�%�@׏�@�*�@�}�@��@�C�@�2a@�h�@��g@�zx@�	l@��/@���@Гu@�)�@ϗ�@�4@��`@�Z�@��@�\�@���@�Z@��@��g@��"@�tT@�,=@��+@�e�@��@ȉ�@�r�@�Z@�(�@�ԕ@�e,@��c@�z�@��@�j@��H@��@�g�@�ѷ@�[�@���@�^�@�[W@�g�@�a�@�C�@���@�{J@�'�@��@�2�@�t�@�@��K@�Ov@�x@��n@�a@�8�@��"@���@�p;@��@��$@�,�@���@���@��P@���@�S�@��a@�iD@�:�@��@�V�@��K@��#@���@�S�@���@���@�~�@��3@�4@��<@�y>@�c @�Z�@�L0@�1'@��>@�w2@���@�Ta@�M@��6@�iD@�	l@�kQ@�	@�ԕ@��C@�v`@�C�@��@�y>@�_@�|�@�L�@�@���@��@���@�\�@��c@�Ĝ@�m�@��@�K�@��@�h�@��@���@�H�@��@���@�z@�@�@��@���@��[@���@�dZ@�&�@��@��]@��x@��Y@�tT@�`�@�0U@���@�c�@�9�@��@���@���@�Ov@�/�@� �@��@��	@�J#@�4�@��@��u@�@�@�4@���@���@���@�IR@��@���@��\@�<�@��A@���@�H�@��@���@�h�@�A�@��]@���@�IR@�!-@�ѷ@���@��z@���@���@���@�+@��[@��\@�Ta@�~@���@��S@�#�@�>�@���@��X@��@�l�@�\�@�*�@�@���@��k@���@�]�@�A @�7L@�!�@��]@�@�@�{@���@���@��F@�k�@�$t@��@���@�I�@�(�@���@��K@��^@�S&@�1�@��@�S@��!@�bN@�J@��@�!-@�@��@���@�V@��@��@��@���@�_p@��@�~(@�-@��W@���@�<6@�%F@��@�a|@�$�@��@�|@�F@��@��?@���@�z�@�z�@�.�@�ϫ@���@��@��@�P�@�-w@��K@��4@�Z�@�(�@��&@���@�(@��K@���@�y>@�Ta@�M�@�C�@�2�@� �@��h@�,�@��@��@���@�d�@�+k@�@��@~�}@~H�@~?@}@}�@|�p@|�@{��@z�1@z6�@y�j@yQ�@x�o@w��@wb�@wS@vȴ@u��@uc�@u(�@t�U@t�@ttT@tU2@s�}@s�@r�x@ra|@r:*@q��@q;@p��@p��@p  @o��@o�@o��@oZ�@n�@n�!@nE�@m��@l�@lM@k��@ko�@k@O@j��@jQ@j�@i�N@i�>@i��@i(�@htT@g�]@g�*@g��@g��@giD@g.I@f�1@fGE@e�@e�S@eS&@dQ�@c�@b��@b�]@bȴ@b��@b�r@bH�@a�n@`�?@_iD@^�8@^��@^s�@^d�@^0U@]ԕ@]�'@\��@\�I@\�@[��@[_p@[!-@Z�!@Y�@Y[W@X�@X�@W�;@W�f@V��@V�x@U�@U�@T*�@T�@Sƨ@R��@R;�@Q��@Q��@Qs�@Qc�@Qc�@QY�@QG�@QA @Q&�@P�?@PbN@O��@Oo�@Oo@N�6@N�@M�@Mhs@L�e@L(�@Ks@K�@J�@Jq�@J.�@I�#@I�@I2a@I;@H�@Hr�@HZ@H>B@H%�@G� @G��@G�V@G��@Gn/@G@F�@F҉@Fxl@E��@EG�@D�@D�o@Db@C��@C�	@C_p@C@B�<@B��@BZ�@A�T@A-w@@�@@�)@@�9@@`�@@-�@?��@?�@?~�@?_p@?@>�B@>��@>}V@>_�@>5?@>!�@=��@=�d@=rG@<�P@<��@<��@<I�@;��@;��@;o�@;�@:��@:�L@:��@:Ta@9��@9G�@8u�@7�m@7��@7>�@7�@6�c@6��@60U@5ϫ@5��@5G�@5+�@4�P@4��@3��@3s@2��@1��@1��@1�@1�@0��@0z�@0D�@07@/�0@/�@@/�@@/W?@/�@.�B@.d�@.�@-��@-��@- \@,��@,��@,tT@,"h@,�@+�@+H�@+ i@*��@*a|@*-@*@)�@)2a@(�?@('R@(  @'�@@'�@&�B@&i�@&O@%�@%|@%G�@%%F@%�@$�$@$V�@$~@#��@#��@#iD@#U�@#J#@#4�@#@#�@"�8@"~�@"8�@"e@!��@!#�@ ��@ ��@ u�@ /�@�@��@�*@y�@A�@,�@�L@!�@��@�n@�@l"@]d@�@�A@�@��@�F@�@@{J@��@�1@s�@d�@3�@�@�@�-@`B@!�@�5@�@�@�o@<�@7@1@�@@F�@�@�@͟@��@GE@�.@�t@�~@}�@A @�f@ی@��@Z@�@�@�g@�0@�4@8@@��@��@c @�@ �@�)@�@��@��@��@��@�7@�7@c@:�@�@�@z�@S�@I�@<�@�@�a@|�@8@@�X@�@l�@Q@&�@e@�Z@�j@�@�z@�n@x�@N<@�@�E@��@��@��@u�@bN@9X@~@�]@˒@��@�q@��@�k@\)@1�@
�8@
��@
��@
�L@
��@
i�@
R�@
{@	�@	�9@	��@	s�@	Y�@	N<@	�@�`@�)@��@`�@ �@	�@�@��@�4@n/@K�@�@��@�c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�:�A�?�A�9�A�=qA�K�A�N�A�N�A�PA�P�A�Q�A�Q�A�N�A�P}A�LdA�L0A�<�A��zA�I�A�A��lA�;0A�~�AݭA��(A�/�A�P�A� 4A��Aـ�A���A��%A�0UAѽ<AΦA̔{A˳hAǜxA��A�#�A�ɺA�4�A��/A�[WA��3A���A�<jA��A���A�4�A���A��A���A�w�A�� A�0!A���A��A��?A��A��A��aA�.IA�=A��TA���A���A���A�ʌA��A���A�6�A�;A���A�u�A��A�QNA��vA�ÖA���A�A|&�Aw�jAtHAoxlAkߤAhA Af��Ab_�A\�HAZ�AX�AU�AQ�mAM�XAL��AKAJ$tAI�AG�AFZ�ACA?~A<�A::*A7�A66A44A2U�A0��A/�A/(A.B�A-�UA-��A,��A,�1A,!�A*�oA)A&�sA&DgA&+�A&;A%Z�A#ɆA"��A!��A �A �!A �A qvA K^A!��A"�{A"
=A!@OA kQA��A�A�AA�AsA��A��A�A�^A�:A�DA�?A�VA��AW�A��A�A�.A1'A��A>BA�>AC�A	A6Av`A�LA*�AԕA{JAjA��A��A��AϫA�A�"A7A�'Aj�A�KA�AA�A
��A
��A
8�A	��A	c�A��At�A�KA
�A
�A	=A	O�A	(A{�AC-A�KA��AtTA4Al�A&A��A_�A1�AGA��A8A�A�A!�A �A w2A Y@���@���@��@�C�@���@�O@���@��@�;�@��@��@�e,@��@�-�@�|�@���@�R�@�G@�_@��@�k�@�~@� \@��@�o�@점@��#@�e,@�;d@� \@���@��@쒣@�kQ@�Ov@��@�]�@�@�'@�Dg@�F@�@��@�Dg@�q@��y@�j@�%�@�a�@�0U@���@��@�J�@�M@��@��@��@�H@��9@��6@�a@���@�c@�c @�x@���@�P�@�
=@��U@�z�@��D@�rG@��@��`@ج@�%�@׏�@�*�@�}�@��@�C�@�2a@�h�@��g@�zx@�	l@��/@���@Гu@�)�@ϗ�@�4@��`@�Z�@��@�\�@���@�Z@��@��g@��"@�tT@�,=@��+@�e�@��@ȉ�@�r�@�Z@�(�@�ԕ@�e,@��c@�z�@��@�j@��H@��@�g�@�ѷ@�[�@���@�^�@�[W@�g�@�a�@�C�@���@�{J@�'�@��@�2�@�t�@�@��K@�Ov@�x@��n@�a@�8�@��"@���@�p;@��@��$@�,�@���@���@��P@���@�S�@��a@�iD@�:�@��@�V�@��K@��#@���@�S�@���@���@�~�@��3@�4@��<@�y>@�c @�Z�@�L0@�1'@��>@�w2@���@�Ta@�M@��6@�iD@�	l@�kQ@�	@�ԕ@��C@�v`@�C�@��@�y>@�_@�|�@�L�@�@���@��@���@�\�@��c@�Ĝ@�m�@��@�K�@��@�h�@��@���@�H�@��@���@�z@�@�@��@���@��[@���@�dZ@�&�@��@��]@��x@��Y@�tT@�`�@�0U@���@�c�@�9�@��@���@���@�Ov@�/�@� �@��@��	@�J#@�4�@��@��u@�@�@�4@���@���@���@�IR@��@���@��\@�<�@��A@���@�H�@��@���@�h�@�A�@��]@���@�IR@�!-@�ѷ@���@��z@���@���@���@�+@��[@��\@�Ta@�~@���@��S@�#�@�>�@���@��X@��@�l�@�\�@�*�@�@���@��k@���@�]�@�A @�7L@�!�@��]@�@�@�{@���@���@��F@�k�@�$t@��@���@�I�@�(�@���@��K@��^@�S&@�1�@��@�S@��!@�bN@�J@��@�!-@�@��@���@�V@��@��@��@���@�_p@��@�~(@�-@��W@���@�<6@�%F@��@�a|@�$�@��@�|@�F@��@��?@���@�z�@�z�@�.�@�ϫ@���@��@��@�P�@�-w@��K@��4@�Z�@�(�@��&@���@�(@��K@���@�y>@�Ta@�M�@�C�@�2�@� �@��h@�,�@��@��@���@�d�@�+k@�@��@~�}@~H�@~?@}@}�@|�p@|�@{��@z�1@z6�@y�j@yQ�@x�o@w��@wb�@wS@vȴ@u��@uc�@u(�@t�U@t�@ttT@tU2@s�}@s�@r�x@ra|@r:*@q��@q;@p��@p��@p  @o��@o�@o��@oZ�@n�@n�!@nE�@m��@l�@lM@k��@ko�@k@O@j��@jQ@j�@i�N@i�>@i��@i(�@htT@g�]@g�*@g��@g��@giD@g.I@f�1@fGE@e�@e�S@eS&@dQ�@c�@b��@b�]@bȴ@b��@b�r@bH�@a�n@`�?@_iD@^�8@^��@^s�@^d�@^0U@]ԕ@]�'@\��@\�I@\�@[��@[_p@[!-@Z�!@Y�@Y[W@X�@X�@W�;@W�f@V��@V�x@U�@U�@T*�@T�@Sƨ@R��@R;�@Q��@Q��@Qs�@Qc�@Qc�@QY�@QG�@QA @Q&�@P�?@PbN@O��@Oo�@Oo@N�6@N�@M�@Mhs@L�e@L(�@Ks@K�@J�@Jq�@J.�@I�#@I�@I2a@I;@H�@Hr�@HZ@H>B@H%�@G� @G��@G�V@G��@Gn/@G@F�@F҉@Fxl@E��@EG�@D�@D�o@Db@C��@C�	@C_p@C@B�<@B��@BZ�@A�T@A-w@@�@@�)@@�9@@`�@@-�@?��@?�@?~�@?_p@?@>�B@>��@>}V@>_�@>5?@>!�@=��@=�d@=rG@<�P@<��@<��@<I�@;��@;��@;o�@;�@:��@:�L@:��@:Ta@9��@9G�@8u�@7�m@7��@7>�@7�@6�c@6��@60U@5ϫ@5��@5G�@5+�@4�P@4��@3��@3s@2��@1��@1��@1�@1�@0��@0z�@0D�@07@/�0@/�@@/�@@/W?@/�@.�B@.d�@.�@-��@-��@- \@,��@,��@,tT@,"h@,�@+�@+H�@+ i@*��@*a|@*-@*@)�@)2a@(�?@('R@(  @'�@@'�@&�B@&i�@&O@%�@%|@%G�@%%F@%�@$�$@$V�@$~@#��@#��@#iD@#U�@#J#@#4�@#@#�@"�8@"~�@"8�@"e@!��@!#�@ ��@ ��@ u�@ /�@�@��@�*@y�@A�@,�@�L@!�@��@�n@�@l"@]d@�@�A@�@��@�F@�@@{J@��@�1@s�@d�@3�@�@�@�-@`B@!�@�5@�@�@�o@<�@7@1@�@@F�@�@�@͟@��@GE@�.@�t@�~@}�@A @�f@ی@��@Z@�@�@�g@�0@�4@8@@��@��@c @�@ �@�)@�@��@��@��@��@�7@�7@c@:�@�@�@z�@S�@I�@<�@�@�a@|�@8@@�X@�@l�@Q@&�@e@�Z@�j@�@�z@�n@x�@N<@�@�E@��@��@��@u�@bN@9X@~@�]@˒@��@�q@��@�k@\)@1�@
�8@
��@
��@
�L@
��@
i�@
R�@
{@	�@	�9@	��@	s�@	Y�@	N<@	�@�`@�)@��@`�@ �@	�@�@��@�4@n/@K�@�@��@�c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	gB	9B	�B	�B	�B	B	�B	MB	9B	B	
=B	
�B	�B	�B	pB	3MB	BB	J�B	Y1B	_pB	r-B	�=B	��B	��B	��B	��B	n�B	<6B	;B	=�B	mCB	��B	��B	�B	��B
�B
%,B
./B
2�B
<�B
^�B
�SB
��B
�YB
�B
�B9B&�B1ABS�BiyBi�BX�B`BBX�Bp�BbNBW�B�B
�B
�9B
�YB
��B
�&B
ϫB
�5B
[�B
RTB
JrB
)B
y�B
��B
�B
�tB
`vB
.�B	��B	��B	�B	�B	�(B	|�B	h�B	X�B	@�B	#�B	�B	B	�B�B��B��B	oB	bB	�B		lB	B��B�B֡BּB��B��B�;BؓB�FB��BچB��B��B��B��B�[B��B	�B	 OB��B�B�lB�lB��B	�B	[B��B�jB		B	SB	$�B	49B	c�B	z^B	y>B	v�B	utB	tnB	q�B	o�B	n�B	k�B	f�B	gRB	oOB	v+B	u�B	~]B	�B	|�B	v�B	v`B	vB	y$B	}�B	��B	��B	�VB	��B	�B	��B	��B	�_B	��B	��B	�RB	��B	�OB	�}B	�]B	��B	��B	�XB	��B	�B	��B	�(B	�B	��B	�.B	�B	�aB	B	ªB	ňB	ƎB	�B	�KB	�BB	�0B	�vB	�=B	�9B	��B	��B	��B	��B	��B	�dB	��B	��B	��B	��B	�B	�B	�DB	�XB	�B	�yB	��B	� B	�pB	�B	�'B	�B	�B	��B	�vB	�B	�[B	�`B	��B	�fB	��B	��B	�4B	��B	�B	�AB	�/B	�kB	�B	�!B	�B	��B	�)B	��B	�1B	ٚB	ؓB	�B	�#B	�B	��B	��B	�IB	�VB	�.B	��B	�BB	�(B	�jB	�0B	�*B	��B	�>B	�B	��B	�FB	��B	�qB	�jB	��B	��B	�B	�-B	�B	�!B	��B	�nB	��B	��B	�RB	��B	��B	�tB	�[B	�WB	�*B	��B	��B	�B	�:B	�TB	�:B	��B	��B	��B	�tB	�B	��B	�B	�B	�tB	�tB	��B	�B	��B	�XB	�*B	�*B	�_B	�_B	�B	�B	�0B	�0B	�B	�kB	�kB	�QB	�qB	��B	� B	�iB	�OB	�B	� B	� B	�B	�5B	�B	�B	�B	�UB	�'B	�B	��B	�AB	�oB	�B	�B	��B	�B	��B	��B	�CB	�wB	��B	��B	�B	��B	��B	�?B	��B	�B	�B	�B	��B	�B	�9B	�B	��B	�zB	��B	�fB	��B	��B	�tB	�B	��B	�?B	�tB	��B	��B	��B	��B	�B	�lB	�rB	��B	��B
 iB
 B
 �B
 B
B
B
oB
oB
AB
uB
uB
�B
�B
-B
GB
�B
�B
MB
�B
B
�B
_B
zB
�B
�B
	B
	�B
	�B

rB

�B

=B

rB

�B
�B
�B
dB
B
�B
jB
�B
�B
�B
B
BB
\B
pB
pB
�B
\B
�B
�B
oB
oB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
B
{B
�B
B
MB
gB
�B
�B
B
9B
mB

B
sB
YB
�B
B
�B
YB

B
?B
YB
�B
�B
sB
�B
_B
B
KB
B
�B
kB
�B
�B
�B
�B
�B
�B
eB
�B
B
qB
�B
B
�B
	B
�B
CB
]B
]B
/B
�B
pB
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
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
# B
$ZB
%zB
%zB
%�B
&B
&�B
&�B
'�B
'RB
'B
'RB
'mB
'mB
'�B
'�B
($B
(>B
'�B
&�B
&�B
'B
'B
'RB
'mB
'�B
'�B
'�B
'�B
(XB
(sB
)*B
*0B
)�B
)�B
)�B
*0B
+B
+kB
+�B
,�B
,�B
-)B
-wB
.B
-�B
-�B
.cB
.�B
/ B
.�B
.�B
/iB
/iB
/�B
0!B
0�B
0�B
0�B
1vB
2GB
2GB
2�B
2�B
3B
3B
33B
3B
3hB
4B
4�B
4�B
4�B
4�B
5ZB
5tB
5ZB
5�B
6FB
6�B
6�B
7B
72B
72B
7B
7�B
8�B
8�B
8�B
8�B
9XB
9�B
:*B
:*B
:B
:�B
:�B
:�B
;JB
;JB
;dB
;JB
;�B
<B
<PB
<PB
<B
<B
;�B
;�B
;�B
;�B
<B
<6B
<B
<B
;�B
;B
;dB
;JB
;dB
;JB
;dB
;�B
;�B
<B
<6B
<B
<B
=�B
?cB
?cB
>�B
>(B
=�B
=�B
=�B
=�B
>wB
?}B
?cB
>�B
>�B
>wB
>�B
?cB
@iB
@iB
@OB
@OB
@OB
@B
@�B
A B
AoB
AUB
AoB
A�B
A�B
B�B
CaB
C-B
C�B
D�B
EmB
E�B
E�B
E�B
F?B
FYB
F�B
G+B
F�B
F�B
GB
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
J#B
J=B
JXB
JrB
J�B
J�B
J�B
J�B
KB
KDB
K�B
K�B
K�B
K�B
K�B
L0B
L~B
LJB
LJB
L�B
L�B
M6B
M�B
M�B
NVB
N<B
NpB
N�B
N�B
N�B
N�B
OB
OB
OBB
OvB
O�B
O�B
PB
PbB
PbB
PHB
PbB
P�B
P�B
Q B
QNB
Q�B
Q�B
RB
RB
R:B
R�B
R�B
R�B
S@B
S�B
TB
TaB
T�B
T�B
T�B
UMB
UB
U2B
UMB
U�B
U�B
VB
V�B
V�B
WYB
W�B
W�B
W�B
WYB
X+B
X�B
X�B
X�B
Y1B
YeB
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
ZQB
Z�B
[=B
[�B
\B
\B
\)B
\]B
\�B
\�B
]/B
]IB
]/B
]IB
]�B
^B
^B
^�B
_;B
_pB
_�B
`BB
`vB
`vB
`vB
`vB
`�B
`�B
`�B
aHB
a�B
a�B
bB
bB
bNB
bNB
b�B
b�B
b�B
b�B
cB
b�B
c:B
cTB
cnB
c�B
c�B
d&B
d&B
d&B
d�B
e,B
e�B
e�B
e�B
f2B
ffB
gB
gmB
g�B
g�B
h$B
h>B
hXB
h�B
h�B
iB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
nB
n�B
o5B
oB
oiB
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
qB
qB
q'B
q[B
q�B
q�B
rB
rB
r-B
rGB
r�B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
t�B
u%B
uZB
utB
u�B
u�B
v+B
v`B
v`B
vzB
v�B
v�B
w2B
wfB
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
y�B
z^B
z�B
z�B
{B
{JB
{dB
{�B
|6B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~B
}�B
~(B
~wB
~wB
~�B
~�B
~�B
HB
}B
�B
�B
�OB
�iB
�iB
�B
�B
�OB
�4B
�OB
��B
��B
�B
��B
�;B
�;B
�oB
��B
��B
�B
�B
�AB
�uB
��B
��B
��B
�-B
�aB
�a111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	gB	SB	�B	�B	�B	B	�B	gB	SB	1B	
XB	B	�B	B	 �B	4�B	C�B	L~B	Z�B	`�B	raB	��B	��B	�AB	��B	�4B	tB	@�B	?B	C�B	p�B	��B	�CB	�B	�rB
KB
&2B
/OB
4�B
A B
a|B
�tB
��B
�RB
�}B
�B1B($B3�BVBlqBl�B[=Bb�B\)BuZBhsBa�B$&B
�ZB
�	B
�JB3B
�XB
�mB
��B
]�B
WsB
Q4B
5B
z*B
�PB
��B
��B
h$B
72B	��B	��B	ѝB	�B	�uB	��B	k�B	^�B	F�B	&�B	�B	�B	B��B�fB�^B	aB	�B	�B	B		B	  B�B�eB�BڠBބB�bBچBՁB��B�qB�dB�|B�B�B�aB�^B	�B	�B��B�RB�	B��B��B	�B	{B	 B��B		B	�B	$&B	2�B	cB	{B	z^B	w�B	v�B	uZB	r|B	q�B	p!B	m�B	g�B	h�B	p�B	w�B	v+B	cB	�B	~]B	w�B	v�B	vzB	y	B	}�B	�[B	��B	��B	�(B	��B	��B	��B	�yB	��B	��B	��B	�VB	��B	� B	��B	�vB	��B	�XB	��B	��B	�[B	��B	��B	��B	��B	�{B	ðB	��B	�GB	�B	�+B	�1B	ǮB	�<B	�KB	�-B	�WB	�B	�FB	�?B	�0B	�B	�B	��B	��B	�BB	�B	�B	�PB	��B	��B	��B	�B	�B	�B	�B	߾B	�vB	�vB	�@B	�B	�vB	��B	�&B	��B	��B	�eB	�B	�B	�B	�hB	��B	�NB	��B	�B	��B	�XB	�VB	��B	��B	��B	چB	ٚB	�B	�B	�_B	�=B	�B	��B	�@B	��B	�<B	�cB	�B	��B	��B	��B	�dB	��B	�DB	��B	�lB	��B	�+B	��B	��B	��B	�B	�XB	��B	�GB	�B	�!B	��B	�B	�B	�zB	�lB	��B	�lB	�FB	�B	�B	��B	�`B	�B	�B	�nB	�B	�B	�B	��B	�,B	��B	�ZB	�B	�&B	�`B	��B	�,B	�RB	��B	�$B	�B	�*B	�*B	�B	�B	��B	��B	�B	�B	�kB	�B	�B	�B	�B	��B	�iB	��B	�iB	�OB	�iB	�OB	�OB	�OB	�5B	�OB	��B	��B	�vB	�B	�[B	�B	��B	�[B	��B	��B	��B	�"B	�)B	�)B	�]B	��B	�B	�B	��B	��B	��B	�ZB	�B	�hB	�B	�?B	��B	�B	�%B	��B	��B	��B	��B	�8B	��B	��B	�B	�?B	��B	��B	��B	�FB	��B	�B	�fB	��B	��B	��B
 B
 �B
 iB
B
UB
�B
[B
�B
�B
[B
uB
�B
�B
�B
�B
�B
�B
�B
�B
B
YB
_B
�B
�B
B
�B
	7B
	�B

	B

�B

�B

rB

�B
B
�B
�B
�B
PB
�B
�B
"B
<B
BB
BB
�B
�B
�B
�B
�B
vB
B
�B
�B
�B
�B
�B
B
uB
�B
�B
�B
�B
�B
,B
FB
�B
2B
2B
�B
�B
�B
�B
9B
SB
�B
?B
�B
sB
�B
EB
�B
�B
?B
YB
sB
�B
�B
�B
B
yB
KB
�B
�B
�B
�B
	B
�B
#B
#B
#B
�B
eB
�B
7B
�B
	B
7B
�B
#B
)B
xB
�B
xB
~B
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
"�B
"�B
"�B
#B
#:B
#B
#B
# B
#:B
$�B
%�B
%�B
%�B
&LB
&�B
&�B
'�B
'mB
'8B
'RB
'mB
'�B
(
B
(
B
(XB
(sB
'�B
'B
&�B
'B
'8B
'�B
'mB
'�B
'�B
'�B
(XB
(�B
(�B
)_B
*KB
*KB
)�B
)�B
*eB
+6B
+�B
,B
,�B
,�B
-]B
-�B
.B
.B
-�B
.�B
/ B
/ B
.�B
/ B
/iB
/�B
/�B
0UB
0�B
0�B
1'B
1�B
2aB
2aB
2�B
2�B
2�B
33B
33B
33B
3�B
4TB
4�B
4�B
4�B
5?B
5�B
5tB
5�B
5�B
6`B
6�B
6�B
72B
7LB
7LB
7LB
8B
8�B
8�B
8�B
9>B
9rB
9�B
:DB
:DB
:DB
:�B
:�B
:�B
;JB
;dB
;B
;dB
;�B
<6B
<PB
<jB
<PB
<B
;�B
;�B
;�B
;�B
<B
<PB
<PB
<6B
;�B
;�B
;�B
;�B
;B
;B
;B
;�B
;�B
<6B
<PB
<B
<B
=�B
?}B
?�B
>�B
>BB
=�B
=�B
=�B
=�B
>�B
?}B
?}B
>�B
>�B
>�B
?B
?�B
@OB
@iB
@OB
@iB
@�B
@iB
AB
A�B
A�B
AoB
A�B
A�B
A�B
B�B
C{B
CGB
D3B
D�B
E�B
E�B
FB
FB
FtB
FtB
F�B
G_B
F�B
F�B
G+B
F�B
GB
F�B
G+B
G�B
G�B
HB
IB
I�B
I�B
J#B
J#B
JXB
JrB
J�B
J�B
J�B
KB
KDB
K^B
K�B
K�B
K�B
K�B
LB
LJB
L�B
LdB
LdB
L�B
L�B
MPB
M�B
NB
NVB
NpB
N�B
N�B
N�B
N�B
OB
OB
OB
O\B
O\B
O�B
O�B
P.B
PbB
P}B
P}B
P�B
P�B
Q B
Q4B
Q�B
Q�B
Q�B
R B
R B
R:B
R�B
R�B
R�B
S[B
S�B
T,B
TaB
T�B
T�B
U2B
UgB
UB
UMB
UgB
U�B
U�B
VB
V�B
W
B
WsB
W�B
W�B
W�B
WsB
XEB
X�B
X�B
Y1B
Y1B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[WB
[�B
\B
\B
\CB
\xB
\�B
\�B
]IB
]IB
]IB
]dB
]�B
^B
^OB
^�B
_VB
_�B
_�B
`BB
`vB
`�B
`vB
`�B
`�B
`�B
`�B
abB
a�B
a�B
b4B
bB
bNB
bhB
b�B
b�B
b�B
b�B
c B
cB
cnB
cnB
cnB
c�B
dB
d@B
d@B
dtB
d�B
eFB
e�B
e�B
e�B
f2B
f�B
gB
g�B
g�B
h
B
h>B
h>B
hsB
h�B
iB
i*B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jB
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
oB
oOB
oB
o�B
oiB
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
p�B
q'B
qB
qAB
q[B
q�B
q�B
r-B
rB
rGB
rGB
r�B
r�B
r�B
sB
shB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
v+B
vzB
vzB
v�B
v�B
wB
wLB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
y�B
y�B
z^B
z�B
z�B
{B
{JB
{B
{�B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
}�B
~(B
~wB
~wB
~�B
~�B
~�B
HB
}B
�B
�B
�OB
��B
�iB
�B
�4B
�iB
�OB
�iB
��B
��B
�B
��B
�;B
�UB
��B
��B
��B
�B
�'B
�[B
�uB
��B
��B
��B
�B
�aB
�a311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.08(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201909010100002019090101000020190901010000202207271132272022072711322720220727113227202207271535032022072715350320220727153503  JA  ARFMdecpA30a                                                                20190821093705  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190821093749  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190821093750  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190821093750  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190821093750  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190821093751  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190821093751                      G�O�G�O�G�O�                JA  ARUP                                                                        20190821095645                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190822000000  CF  PSAL_ADJUSTED_QC@z�@z�G�O�                JM  ARCAJMQC2.0                                                                 20190831160000  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190831160000  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023227  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063503  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                