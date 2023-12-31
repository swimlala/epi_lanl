CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-20T17:02:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170920170225  20190405100807  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�'�z(1   @�'���&�@-�l�C���d�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C�C  C�fC"  C$  C&  C(  C*  C,  C.  C0�C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�  D�<�D�vfD��fD�3D�P D���D���D���D�33D�s3DǶfD��3D�L�D�y�D��D�fD�P D�vfD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�=qA	�A)�AI�Ai�A��\A��\A�A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�BrG�BzG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C xRC"��C$��C&��C(��C*��C,��C.��C0��C2��C4xRC6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Dt�HDy�HD�=D�O
D���D�ȣD�%pD�b=D��
D��
D�
D�EpD��pD�ȣD��pD�_
Dڋ�D��
D�(�D�b=D�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA���A�ĜA���A���A���A���A���A���A��
A��A���A���A�\)A�A�A�=qA�;dA�7LA�33A�1'A�/A�1'A�1'A�33A�1'A�-A�(�A�(�A�&�A�$�A��A��A��A�oA�
=A�  A�ĜA��A�A�A݅A���A�9XA�bA��AّhA�-A�
=A�%A���A�~�AԶFA�JAґhAГuA�l�A�9XȀ\A�^5A�%A�n�A���A�`BA�K�A�"�A�z�Aŏ\A�~�A��A�dZA��
A�ƨA�Q�A��A���A���A�&�A���A�9XA���A��A���A�v�A�ȴA�VA�t�A�1'A�A�A�A�x�A��A���A���A��A�VA�"�A��^A�bNA��A���A�`BA��
A�{A�JAz�+AxE�Av�HAtVApn�Ah��Ad1A`��A[�AV9XARE�AOXAK�AF�ACVA@ffA>bA<�uA;oA9p�A8�RA81'A7�A7ƨA7��A7��A7��A7�7A7\)A6�yA6�A6�\A6n�A61A5;dA4�uA4VA3�wA2A0�DA1x�A1�FA1�wA1��A0�\A0JA0�uA0��A0�A0$�A/�PA/VA.�yA0�A/�A.�+A.bA-t�A+�A*��A)��A)hsA)O�A(��A(��A'��A&ȴA%��A%7LA%"�A%VA$v�A"��A"�!A"�A!�wA!7LA ZA`BA��AG�A�Ap�A��AI�A\)A;dAoA��A��A(�A��A�A�PA��A-A1A�TAƨAt�A�AA�yA��A�DAJA��A"�AM�A�A�hA^5AA��A7LA�/A{Ap�A�HA�`A��A5?A33A
��A
jA
��A
��A
�9A
�uA
bNA	�mA	?}AQ�A�hA��AZA��A�AAG�AC�AVA��A�A��A�+A$�AAXA7LA&�A M�@�t�@�5?@�J@���@���@�x�@��u@�l�@�{@���@�p�@�V@�1'@�dZ@���@�n�@�V@���@�%@�w@�-@��@���@���@띲@�+@�7@��@�9X@���@���@�u@蛦@�(�@�b@�1@�1@�  @�@��@�@�ff@���@�?}@�z�@�9X@��
@�C�@◍@�7L@���@��u@�I�@�  @��y@�E�@���@�Z@�l�@�o@�
=@��@��@���@�^5@��@٩�@ى7@��@؋D@�Q�@���@�S�@�^5@�O�@�O�@�X@�hs@�V@ԛ�@�b@�E�@�Z@�dZ@·+@Ͳ-@̴9@��m@˝�@�;d@ʗ�@�$�@�`B@ȣ�@�\)@���@Ƈ+@��@ũ�@�7L@�Z@� �@Õ�@�@��@��@��T@���@��-@��@��@���@��m@��P@���@���@�l�@�;d@��y@��R@��!@�ff@�@�p�@���@�z�@� �@��@�ȴ@��\@�V@��@��#@���@�p�@�G�@�%@���@�z�@�9X@���@��F@�|�@�K�@�
=@�ȴ@�E�@��T@���@�`B@�&�@��@��@�1@��w@�l�@�C�@�"�@���@��+@�5?@���@��^@���@��h@�x�@�%@��u@���@���@�|�@�+@��@�ff@�{@��-@�Ĝ@�Z@��@���@��@�\)@�"�@��@��@��R@��\@�=q@�-@���@�?}@���@�Q�@�1@���@���@���@�=q@�x�@� �@��F@�C�@�
=@��@�n�@��-@��@��@���@��w@��@���@��P@�l�@�K�@�K�@�o@���@��@���@�^5@���@�p�@�O�@�&�@��9@�j@��;@�S�@��@�E�@��@��#@���@�j@�O�@��F@��@�33@~E�@t(�@m�@e�-@Z-@R�@LZ@E��@=�@7
=@.�@(bN@�@-@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ȴA���A�ĜA���A���A���A���A���A���A��
A��A���A���A�\)A�A�A�=qA�;dA�7LA�33A�1'A�/A�1'A�1'A�33A�1'A�-A�(�A�(�A�&�A�$�A��A��A��A�oA�
=A�  A�ĜA��A�A�A݅A���A�9XA�bA��AّhA�-A�
=A�%A���A�~�AԶFA�JAґhAГuA�l�A�9XȀ\A�^5A�%A�n�A���A�`BA�K�A�"�A�z�Aŏ\A�~�A��A�dZA��
A�ƨA�Q�A��A���A���A�&�A���A�9XA���A��A���A�v�A�ȴA�VA�t�A�1'A�A�A�A�x�A��A���A���A��A�VA�"�A��^A�bNA��A���A�`BA��
A�{A�JAz�+AxE�Av�HAtVApn�Ah��Ad1A`��A[�AV9XARE�AOXAK�AF�ACVA@ffA>bA<�uA;oA9p�A8�RA81'A7�A7ƨA7��A7��A7��A7�7A7\)A6�yA6�A6�\A6n�A61A5;dA4�uA4VA3�wA2A0�DA1x�A1�FA1�wA1��A0�\A0JA0�uA0��A0�A0$�A/�PA/VA.�yA0�A/�A.�+A.bA-t�A+�A*��A)��A)hsA)O�A(��A(��A'��A&ȴA%��A%7LA%"�A%VA$v�A"��A"�!A"�A!�wA!7LA ZA`BA��AG�A�Ap�A��AI�A\)A;dAoA��A��A(�A��A�A�PA��A-A1A�TAƨAt�A�AA�yA��A�DAJA��A"�AM�A�A�hA^5AA��A7LA�/A{Ap�A�HA�`A��A5?A33A
��A
jA
��A
��A
�9A
�uA
bNA	�mA	?}AQ�A�hA��AZA��A�AAG�AC�AVA��A�A��A�+A$�AAXA7LA&�A M�@�t�@�5?@�J@���@���@�x�@��u@�l�@�{@���@�p�@�V@�1'@�dZ@���@�n�@�V@���@�%@�w@�-@��@���@���@띲@�+@�7@��@�9X@���@���@�u@蛦@�(�@�b@�1@�1@�  @�@��@�@�ff@���@�?}@�z�@�9X@��
@�C�@◍@�7L@���@��u@�I�@�  @��y@�E�@���@�Z@�l�@�o@�
=@��@��@���@�^5@��@٩�@ى7@��@؋D@�Q�@���@�S�@�^5@�O�@�O�@�X@�hs@�V@ԛ�@�b@�E�@�Z@�dZ@·+@Ͳ-@̴9@��m@˝�@�;d@ʗ�@�$�@�`B@ȣ�@�\)@���@Ƈ+@��@ũ�@�7L@�Z@� �@Õ�@�@��@��@��T@���@��-@��@��@���@��m@��P@���@���@�l�@�;d@��y@��R@��!@�ff@�@�p�@���@�z�@� �@��@�ȴ@��\@�V@��@��#@���@�p�@�G�@�%@���@�z�@�9X@���@��F@�|�@�K�@�
=@�ȴ@�E�@��T@���@�`B@�&�@��@��@�1@��w@�l�@�C�@�"�@���@��+@�5?@���@��^@���@��h@�x�@�%@��u@���@���@�|�@�+@��@�ff@�{@��-@�Ĝ@�Z@��@���@��@�\)@�"�@��@��@��R@��\@�=q@�-@���@�?}@���@�Q�@�1@���@���@���@�=q@�x�@� �@��F@�C�@�
=@��@�n�@��-@��@��@���@��w@��@���@��P@�l�@�K�@�K�@�o@���@��@���@�^5@���@�p�@�O�@�&�@��9@�j@��;@�S�@��@�E�@��@��#G�O�@�j@�O�@��F@��@�33@~E�@t(�@m�@e�-@Z-@R�@LZ@E��@=�@7
=@.�@(bN@�@-@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
L�B
K�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
I�B
I�B
H�B
G�B
F�B
E�B
D�B
<jB
)�B
�B
1B	��B	��B	�B	�mB	�`B	�NB	�BB	�;B	�ZB	�B
B
PB
5?B
p�B
��B
�/B
�B
��B
��B
��B �B7LBK�BXBiyB�%B��B��B�3B�LB�RB�B�3B�wB�B�;B��B��BB�B�B\BJBB��B�B��B��B� BN�B�B
��B
��B
�B
��B
�bB
gmB
S�B
D�B
1'B
�B
  B	��B	��B	��B	�VB	~�B	gmB	A�B	,B	�B	VB	B��B�B�B�B��B	PB	�B	�B	0!B	K�B	\)B	ffB	l�B	p�B	u�B	u�B	u�B	u�B	w�B	}�B	�B	�B	�B	�+B	�PB	�bB	��B	��B	�9B	�5B	��B
B
B
B
%B
{B
(�B
6FB
8RB
33B
.B
+B
.B
N�B
`BB
ZB
VB
O�B
B�B
B�B
<jB
<jB
=qB
?}B
E�B
B�B
F�B
G�B
I�B
J�B
H�B
F�B
G�B
H�B
G�B
E�B
C�B
?}B
A�B
C�B
E�B
D�B
A�B
?}B
=qB
>wB
A�B
A�B
A�B
A�B
A�B
@�B
@�B
?}B
>wB
@�B
A�B
A�B
A�B
A�B
@�B
?}B
?}B
>wB
<jB
:^B
6FB
33B
2-B
1'B
0!B
+B
'�B
$�B
!�B
 �B
 �B
 �B
'�B
(�B
%�B
"�B
�B
�B
�B
 �B
'�B
+B
.B
/B
,B
&�B
�B
�B
{B
oB
{B
�B
�B
�B
�B
�B
\B
\B
\B
JB

=B
1B
1B
1B
+B
+B
%B
1B
+B
+B
+B
+B
+B
+B
B
B
B
  B	��B	��B
B
B
B
  B	��B	��B	�B	�B	�BB	�)B	�#B	�B	�
B	�
B	��B	��B	�B	�;B	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�sB	�fB	�TB	�BB	�;B	�/B	�)B	�#B	�B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�BB	�HB	�TB	�TB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�NB	�NB	�NB	�NB	�BB	�/B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�TB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
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
%B
B
B
B
B
B
B
B
  B
  B
  B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
	7B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
%B
%B
%B
+B
DB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
!�B
+B
1'B
9XB
@�B
D�B
E�B
J�B
O�B
S�B
YB
\)B
`BB
e`B
k�B
o�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
L�B
K�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
I�B
I�B
H�B
G�B
F|B
EvB
DqB
<?B
)�B
eB
B	��B	��B	�mB	�BB	�4B	�&B	�B	�B	�,B	�B
 �B
%B
5B
pxB
��B
�B
�bB
��B
�B
��B �B7BK�BW�BiJB��B��B��B�B�B�"B��B�B�HB��B�B��B��B�BaBpB+BB�B��B�dB˓B��B�BN�B]B
��B
��B
��B
�hB
�*B
g7B
S�B
DcB
0�B
_B	��B	ҹB	��B	�RB	�B	~�B	g3B	AMB	+�B	zB	B	�B��B�wB�KB�XB��B	B	HB	~B	/�B	K�B	[�B	f%B	lKB	pbB	u�B	u�B	u�B	u�B	w�B	}�B	��B	��B	��B	��B	�B	�B	�WB	�~B	��B	��B	��B
�B
�B
�B
�B
8B
(�B
6B
8B
2�B
-�B
*�B
-�B
N�B
`B
Y�B
U�B
O�B
BLB
BOB
<(B
<)B
=.B
?<B
E_B
BOB
FhB
GlB
IyB
J�B
HrB
FhB
GkB
HqB
GkB
E`B
CSB
?7B
AEB
CTB
E`B
DYB
AIB
?;B
=-B
>4B
AEB
AFB
ACB
AEB
ADB
@=B
@AB
?8B
>4B
@>B
AEB
ADB
ADB
AFB
@?B
?9B
?9B
>4B
<&B
:B
6B
2�B
1�B
0�B
/�B
*�B
'�B
$�B
!�B
 �B
 B
 �B
'�B
(�B
%�B
"�B
gB
[B
bB
 �B
'�B
*�B
-�B
.�B
+�B
&�B
yB
TB
7B
+B
6B
QB
RB
HB
HB
?B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B
 �B
 �B
�B	��B	��B	��B	�iB	�BB	��B	��B	��B	��B	��B	��B	ԷB	ӰB	��B	��B	�B	�B	�B	�B	�B	� B	�'B	�,B	�.B	�1B	�2B	�2B	�3B	�4B	�-B	�"B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	պB	սB	վB	��B	��B	��B	��B	��B	�B	�B	� B	�/B	�7B	�5B	�>B	�=B	�=B	�FB	�JB	�OB	�dB	�fB	�fB	�aB	�aB	�gB	�nB	�|B	�|B	�uB	�yB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
 B
 B
%B
$B
(B
+B
,B
3B
4B
1G�O�B
9B
PB
!�B
*�B
0�B
9B
@8B
DSB
EWB
JyB
O�B
S�B
X�B
[�B
_�B
eB
k;B
oRB
trB
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008072019040510080720190405100807  AO  ARCAADJP                                                                    20170920170225    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170920170225  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170920170225  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100807  IP                  G�O�G�O�G�O�                