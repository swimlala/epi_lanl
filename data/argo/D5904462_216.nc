CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-25T17:02:40Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170925170240  20190405100807  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�(�"331   @�(�5�l@-�dZ��dȼj~��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C �C33C�fC�fC  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP�CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�fD�Y�D�� D�� D�fD�L�D�� D��fD��D�P D��fDǼ�D�3D�@ Dڜ�D�� D��D�9�D�i�D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�=qA	�A)�AI�Ai�A��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�Br�BzG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C�CxRCxRC��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CLxRCN��CP��CR��CT��CV��CXxRCZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�U�C�b�C�<)C�<)C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Dt�Dy�D�(�D�k�D��=D��=D��D�_
D��=D��D�+�D�b=D���D��
D�%pD�R=Dگ
D��=D�
D�K�D�{�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�33A�5?A�(�A�%A��A�A�A�"�A��A�{A�oA�JA�JA�
=A�
=A�%A�A�A�  A���A���A���A��A��A��A��A��mA��#A���A�A���A��DA�^5A�t�A�ZA܍PA�I�AپwA�bNA�jAվwA�I�A�oAԝ�A�A�A�1A�|�A�&�AҴ9A�|�A�9XA��TA�+AЇ+A��TA�oAΙ�A�ƨA��A�Q�A˙�Aɧ�A���A��TA��A��hA�=qA�z�A���A�1'A��\A�I�A�"�A���A���A��\A�A���A��A���A�x�A�r�A�n�A���A��A��!A���A�hsA�x�A�jA��A��A��yA�E�A��+A�7LA�z�A��9A�~�A�G�A��A�l�A���A�O�A�bNA�A��+A��9A}Az�uAw\)Au�
At��ArE�An�RAmAkVAgt�Ae/A`�/A[x�AX��AS�AQ�TAP=qAKC�AIAGVAE��ACdZAA�mA@�RA>v�A;�FA:ffA8$�A6Q�A5?}A4��A49XA3�A2��A0JA/dZA/�A.�9A-��A-�A-O�A,��A+�A+\)A+;dA+�A*z�A*bA*9XA)�A)�A(A�A'�
A'ƨA'�hA'l�A'G�A'�A&�A&E�A&=qA%��A%��A$bA#\)A#�A"�`A"ZA!��A!��A ĜA   A�jAl�AC�A�`AbNA�wA\)A�AƨA�/A�Av�A�A"�A��AI�A�A�7A+A^5AJA��A��AG�A&�A�A�\A1A�A�AhsA\)AG�A/AoA�/A�\Az�An�AE�A  AƨA�FA\)A��A��A��A��A��A�uAZA�AC�A�A�`A�9A�!A��A�-AG�A/A
��A
z�A
ffA
-A
1A	�TA	��A	�^A	��A	l�A	�A��A|�A�FA��A�A�A
=AbNA��A��A&�A ��A (�@�S�@�x�@��/@���@��@�Q�@�t�@��R@��!@��\@�@��u@��@��
@��H@�v�@��@���@�9X@���@�l�@�V@�x�@��@�K�@��@��@�+@�{@���@�j@�~�@�x�@��`@�Q�@�  @�P@�ƨ@��@�w@�P@�"�@��y@�ȴ@�v�@���@���@�9@�z�@�A�@�@�|�@�;d@��@�v�@�J@�^@�j@��@���@߅@�;d@�n�@��#@�G�@�%@��@�~�@ٲ-@��/@׍P@�-@�Z@�;d@���@��y@ҸR@�$�@���@Ѻ^@�7L@�j@�ƨ@�\)@��@�
=@�@�ff@̴9@�9X@˝�@��@�^5@�5?@��@ɲ-@Ɂ@�p�@�`B@�?}@�%@ȼj@ȋD@�  @��m@Ǿw@�C�@Ə\@�J@�p�@�V@���@ēu@�(�@��
@�l�@�+@��y@�ȴ@�n�@��@���@��@��#@��^@���@�O�@��@��u@�j@��@��F@���@�S�@�ȴ@�5?@��@��^@��@�p�@�hs@�G�@�/@��`@���@��@�9X@���@��@��P@�|�@�"�@��@�-@���@���@�(�@�  @� �@�Z@��m@��w@�9X@�Z@��@��m@�ƨ@��F@�t�@�C�@���@��+@��^@��@���@�j@�A�@�9X@�(�@��F@���@�V@��@��@�&�@�V@�z�@��@�"�@��!@�n�@���@�Q�@�1@�1@��m@�|�@��H@�-@�7L@�1'@���@�;d@���@��R@��!@��!@��R@���@���@���@�hs@�&�@���@�r�@�ƨ@�C�@�
=@���@�^5@�5?@�`B@��u@���@�  @�=q@���@�S�@�^5@y��@r-@g+@]�@R��@J-@B�\@<1@0bN@*-@"�@�@�9@��@��@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�9XA�33A�5?A�(�A�%A��A�A�A�"�A��A�{A�oA�JA�JA�
=A�
=A�%A�A�A�  A���A���A���A��A��A��A��A��mA��#A���A�A���A��DA�^5A�t�A�ZA܍PA�I�AپwA�bNA�jAվwA�I�A�oAԝ�A�A�A�1A�|�A�&�AҴ9A�|�A�9XA��TA�+AЇ+A��TA�oAΙ�A�ƨA��A�Q�A˙�Aɧ�A���A��TA��A��hA�=qA�z�A���A�1'A��\A�I�A�"�A���A���A��\A�A���A��A���A�x�A�r�A�n�A���A��A��!A���A�hsA�x�A�jA��A��A��yA�E�A��+A�7LA�z�A��9A�~�A�G�A��A�l�A���A�O�A�bNA�A��+A��9A}Az�uAw\)Au�
At��ArE�An�RAmAkVAgt�Ae/A`�/A[x�AX��AS�AQ�TAP=qAKC�AIAGVAE��ACdZAA�mA@�RA>v�A;�FA:ffA8$�A6Q�A5?}A4��A49XA3�A2��A0JA/dZA/�A.�9A-��A-�A-O�A,��A+�A+\)A+;dA+�A*z�A*bA*9XA)�A)�A(A�A'�
A'ƨA'�hA'l�A'G�A'�A&�A&E�A&=qA%��A%��A$bA#\)A#�A"�`A"ZA!��A!��A ĜA   A�jAl�AC�A�`AbNA�wA\)A�AƨA�/A�Av�A�A"�A��AI�A�A�7A+A^5AJA��A��AG�A&�A�A�\A1A�A�AhsA\)AG�A/AoA�/A�\Az�An�AE�A  AƨA�FA\)A��A��A��A��A��A�uAZA�AC�A�A�`A�9A�!A��A�-AG�A/A
��A
z�A
ffA
-A
1A	�TA	��A	�^A	��A	l�A	�A��A|�A�FA��A�A�A
=AbNA��A��A&�A ��A (�@�S�@�x�@��/@���@��@�Q�@�t�@��R@��!@��\@�@��u@��@��
@��H@�v�@��@���@�9X@���@�l�@�V@�x�@��@�K�@��@��@�+@�{@���@�j@�~�@�x�@��`@�Q�@�  @�P@�ƨ@��@�w@�P@�"�@��y@�ȴ@�v�@���@���@�9@�z�@�A�@�@�|�@�;d@��@�v�@�J@�^@�j@��@���@߅@�;d@�n�@��#@�G�@�%@��@�~�@ٲ-@��/@׍P@�-@�Z@�;d@���@��y@ҸR@�$�@���@Ѻ^@�7L@�j@�ƨ@�\)@��@�
=@�@�ff@̴9@�9X@˝�@��@�^5@�5?@��@ɲ-@Ɂ@�p�@�`B@�?}@�%@ȼj@ȋD@�  @��m@Ǿw@�C�@Ə\@�J@�p�@�V@���@ēu@�(�@��
@�l�@�+@��y@�ȴ@�n�@��@���@��@��#@��^@���@�O�@��@��u@�j@��@��F@���@�S�@�ȴ@�5?@��@��^@��@�p�@�hs@�G�@�/@��`@���@��@�9X@���@��@��P@�|�@�"�@��@�-@���@���@�(�@�  @� �@�Z@��m@��w@�9X@�Z@��@��m@�ƨ@��F@�t�@�C�@���@��+@��^@��@���@�j@�A�@�9X@�(�@��F@���@�V@��@��@�&�@�V@�z�@��@�"�@��!@�n�@���@�Q�@�1@�1@��m@�|�@��H@�-@�7L@�1'@���@�;d@���@��R@��!@��!@��R@���@���@���@�hs@�&�@���@�r�@�ƨ@�C�@�
=@���@�^5@�5?@�`B@��uG�O�@�  @�=q@���@�S�@�^5@y��@r-@g+@]�@R��@J-@B�\@<1@0bN@*-@"�@�@�9@��@��@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
6FB
6FB
6FB
6FB
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
6FB
49B
33B
1'B
,B
�B	��B	�B	�ZB	��B	�^B	�XB	�jB	��B	B	ȴB	��B	��B	�#B	�NB	�B	�B	�B	��B
VB
�B
1'B
K�B
VB
k�B
� B
��B
�B
�HBJB>wBx�B��B�}B��B�)B��B��B
=B�B�B#�B33B<jBB�BH�B=qB8RB1'B-B$�B�B�BJBB��B��B�B�B�`B�/B��B��B��B~�B5?B
�B
��B
�7B
n�B
8RB
-B
$�B
DB	�sB	ȴB	�?B	��B	��B	�oB	�B	o�B	e`B	XB	G�B	:^B	%�B	�B	PB	B��B��B��B	  B	B	1B	bB	�B	�B	"�B	.B	9XB	G�B	Q�B	[#B	bNB	n�B	r�B	v�B	}�B	� B	�B	�B	�%B	�+B	�1B	�DB	��B	�B	�!B	�-B	�qB	��B	�;B	�B	�yB	�B
B
PB
hB
uB
�B
�B
�B
$�B
&�B
'�B
(�B
5?B
9XB
9XB
:^B
;dB
<jB
<jB
=qB
=qB
>wB
?}B
?}B
>wB
>wB
>wB
=qB
<jB
=qB
;dB
9XB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
?}B
>wB
=qB
=qB
<jB
=qB
<jB
=qB
<jB
<jB
<jB
9XB
8RB
5?B
49B
49B
33B
2-B
1'B
1'B
0!B
0!B
0!B
/B
/B
0!B
/B
0!B
0!B
0!B
0!B
/B
.B
+B
%�B
"�B
#�B
#�B
"�B
"�B
"�B
$�B
$�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
uB
uB
oB
bB
PB
PB
VB
\B
\B
VB
PB
JB
	7B
%B
B
%B
1B
1B
	7B
VB
hB
�B
�B
�B
{B
{B
uB
uB
oB
oB
hB
bB
bB
bB
\B
VB
VB
VB
VB
PB
JB
DB
DB

=B
1B
%B
B
B	��B	��B	��B	�B	�B	�B	�`B	�NB	�HB	�HB	�HB	�ZB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
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
B
%B
+B
1B
1B
1B
1B
	7B
	7B
1B
1B

=B
JB
JB
PB
PB
PB
JB
JB
VB
VB
PB
PB
PB
PB
PB

=B
+B
B
B
%B
%B
%B
%B
%B
%B
%B
B
%B
+B
+B
1B
1B
1B
	7B
1B
1B
1B

=B
JB
\B
oB
�B
%�B
-B
5?B
<jB
A�B
D�B
H�B
O�B
S�B
W
B
\)B
e`B
iyB
n�B
q�B
u�B
z�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
6B
6B
6B
6B
5B
6B
7B
7B
7B
7"B
7 B
7 B
7B
7B
7 B
8(B
8#B
8%B
8%B
8%B
8(B
8#B
8#B
8%B
8%B
8(B
7"B
7!B
6B
4B
3B
0�B
+�B
aB	��B	�RB	�/B	ʕB	�3B	�-B	�=B	�UB	�cB	ȋB	̠B	ϴB	��B	�"B	�XB	�kB	�B	��B
&B
�B
0�B
K�B
U�B
kZB
�B
�aB
��B
�BB>FBx�B�]B�KB��B��B�B��B
	B\B|B#�B2�B<8BB]BH�B=?B8B0�B,�B$�B�B^BB�B��B�B�vB�QB�-B��BһB�MB�xB~�B5B
��B
��B
� B
n`B
8B
,�B
$�B
B	�:B	�yB	�B	��B	�hB	�5B	��B	oaB	e%B	W�B	GqB	:"B	%�B	EB	B	 �B��B��B��B��B	�B	�B	$B	GB	ZB	"�B	-�B	9B	GoB	Q�B	Z�B	bB	nVB	rnB	v�B	}�B	�B	��B	��B	��B	��B	��B	�B	�ZB	��B	��B	��B	�/B	ΗB	��B	�=B	�6B	�iB
�B
B
(B
2B
?B
PB
zB
$�B
&�B
'�B
(�B
5B
9B
9B
:B
;!B
<'B
<&B
=,B
=0B
>2B
?9B
?9B
>2B
>3B
>0B
=/B
<(B
=.B
; B
9B
8B
8B
9B
9B
9B
:B
:B
9B
:B
;B
;B
; B
<&B
<%B
=,B
>3B
?8B
@AB
@@B
@>B
@?B
@>B
@@B
@=B
@@B
ACB
AEB
@?B
?7B
>6B
=+B
=+B
<%B
=.B
<&B
=*B
<$B
<$B
<'B
9B
8B
4�B
3�B
3�B
2�B
1�B
0�B
0�B
/�B
/�B
/�B
.�B
.�B
/�B
.�B
/�B
/�B
/�B
/�B
.�B
-�B
*�B
%�B
"�B
#�B
#�B
"�B
"�B
"�B
$�B
$�B
"�B
 ~B
zB
lB
aB
_B
`B
YB
SB
RB
IB
LB
NB
HB
BB
:B
;B
5B
/B
/B
0B
,B
-B
)B
B
B

B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
B
!B
;B
:B
:B
4B
4B
.B
.B
(B
&B
B
B
B
B
B
B
B
B
B
B
B

�B

�B
	�B
�B
�B
�B
�B	��B	��B	��B	�nB	�PB	�7B	�B	�B	��B	��B	� B	�B	�B	�&B	�)B	�'B	�-B	�1B	�1B	�1B	�2B	�/B	�=B	�<B	�CB	�EB	�@B	�CB	�BB	�AB	�DB	�DB	�BB	�CB	�BB	�BB	�CB	�BB	�CB	�@B	�CB	�BB	�DB	�DB	�GB	�JB	�HB	�GB	�OB	�PB	�OB	�RB	�NB	�RB	�NB	�OB	�RB	�RB	�VB	�PB	�OB	�YB	�OB	�OB	�VB	�TB	�UB	�VB	�WB	�VB	�TB	�WB	�\B	�\B	�\B	�`B	�bB	�bB	�gB	�zB	�}B	��B	��B	�yB	�zB	�{B	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
 B
B
B
	B
B
�B
B
B
B
B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
 G�O�B
%B
DB
%�B
,�B
4�B
<B
A=B
DRB
HjB
O�B
S�B
V�B
[�B
eB
i/B
nMB
q^B
uzB
z�B
~�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008072019040510080720190405100807  AO  ARCAADJP                                                                    20170925170240    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170925170240  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170925170240  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100807  IP                  G�O�G�O�G�O�                