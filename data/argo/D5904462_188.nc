CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:59Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125959  20190405100802  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���� ,1   @�����&@-(�\)�c��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyl�D��D�C3D���D�� D�  D�P D�y�D��fD�3D�L�D�i�D��3D��D�L�D�33D�� D� D�6fD�c3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=qA	�A)�AI�Ai�A��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�BrG�BzG�B�#�B�#�B�#�B�#�B�#�B�#�B��qB��B�#�B��B�#�B�#�B�#�B�W
B�#�B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B��B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU��DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�Dy�HD�
D�UpD���D��=D�2=D�b=D���D��D�pD�_
D�{�D��pD�+�D�_
D�EpD��=D�"=D�H�D�upD�B=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AҸRA�p�A�Q�A�?}A�5?A�-A��A�1A��HA���A�ȴA�AѺ^AѸRAѴ9AѲ-AѬAѧ�Aѥ�Aѡ�Aѝ�Aї�A�G�A�1A��A���A�~�A�{AΣ�A�A�A�ƨA�$�A��A��HA̺^A˓uA���AɅAƥ�A�p�A��A�~�A�bA�\)A���A�I�A��HA���A���A�
=A�=qA�{A��uA���A���A�r�A���A��A��^A��yA�r�A��A�G�A�/A���A��A�C�A��-A�?}A�9XA�~�A���A�  A�/A��A��-A��wA�A���A�A��!A�A���A���A���A��hA�-A��A�?}A��`A� �A�(�A�S�A�1A~�9A~z�A}VAxQ�AvE�Av$�Av{AuK�Ar9XAm�TAgXAb�A_x�AZVAU��ARJAP5?AK/AHȴAG��AF�ABJA=�mA;��A:�A9S�A8��A6�A5��A5
=A3t�A2�A2bA1XA0�A0�uA0E�A0  A/C�A,�HA+ƨA*^5A)A)A)�A*{A*$�A*$�A)��A*z�A+hsA)�;A(v�A(A�A'&�A&5?A%�mA%�hA%%A%%A$�A#�A"�9A"~�A"^5A!��A!�A!��A!��A!t�A!�A �yA VA�A��AhsA�`A��Az�A��A�RAbNAbA��An�A=qAVA=qA$�A(�A  A�-A;dA�A-A�A;dA��A��A�PA�At�AC�A�!A��A�A&�A�AQ�A33A��A��Az�A$�A�A�`A$�A"�A�#AhsA�A
9XA	/A�/A�jA��A��A��A^5AI�A��A|�A"�AA�A�TAdZAXA"�A�A�jAr�A1A�
A��Ap�A ��A 9X@�33@��+@�v�@�V@���@�C�@���@�^5@��@�V@��j@�z�@��;@��@���@�5?@��@���@��m@��@�v�@�h@�r�@� �@�1@���@��@@�=q@�?}@��@�K�@�@�E�@��@�t�@��@�R@�M�@��T@�@��`@�t�@�E�@��/@�1@�t�@���@�E�@��#@ܛ�@ۮ@�9X@�r�@���@���@ܬ@�bN@۾w@�dZ@�;d@�v�@��@�V@�  @�S�@���@�p�@���@Լj@�A�@�dZ@�~�@��@�p�@�&�@�I�@ϥ�@��H@Χ�@�n�@��#@�?}@��@��@�%@���@̛�@�(�@˶F@�dZ@ʸR@�n�@�V@�J@ɲ-@�7L@ȼj@�1'@���@���@ǅ@�"�@��H@�v�@���@�/@��`@Ĵ9@�z�@�b@�l�@��@���@�=q@���@��h@�G�@��@��`@�j@��@��P@�33@�ȴ@��\@�E�@�{@���@��#@�p�@�V@�Ĝ@�j@�Q�@�  @�C�@�ȴ@�ff@��-@�O�@�Ĝ@�I�@���@��@�t�@��y@��\@��@�x�@�%@�Ĝ@�z�@�9X@���@�o@�ȴ@�~�@���@��7@���@���@�1@�\)@��@�ff@�$�@�J@���@���@��/@�I�@��w@�;d@�"�@���@�~�@��#@��@�Z@�  @��;@�;d@�
=@���@���@�n�@�5?@�-@��#@�x�@�X@���@��@�1@���@���@�"�@�n�@�@��#@���@�@��T@���@��@��@��j@��@���@�dZ@���@��h@���@���@���@��@��j@���@���@���@���@�Ĝ@���@�Z@��@��@�K�@�"�@�ȴ@��\@�@���@���@�hs@��/@��u@�Z@���@��@�\)@���@��H@��@���@��!@�~�@��;@�`B@��P@��9@y��@nE�@d�j@[��@R�H@IX@A��@9G�@0Ĝ@)X@"�\@�-@�@1@5?@
�@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AҸRA�p�A�Q�A�?}A�5?A�-A��A�1A��HA���A�ȴA�AѺ^AѸRAѴ9AѲ-AѬAѧ�Aѥ�Aѡ�Aѝ�Aї�A�G�A�1A��A���A�~�A�{AΣ�A�A�A�ƨA�$�A��A��HA̺^A˓uA���AɅAƥ�A�p�A��A�~�A�bA�\)A���A�I�A��HA���A���A�
=A�=qA�{A��uA���A���A�r�A���A��A��^A��yA�r�A��A�G�A�/A���A��A�C�A��-A�?}A�9XA�~�A���A�  A�/A��A��-A��wA�A���A�A��!A�A���A���A���A��hA�-A��A�?}A��`A� �A�(�A�S�A�1A~�9A~z�A}VAxQ�AvE�Av$�Av{AuK�Ar9XAm�TAgXAb�A_x�AZVAU��ARJAP5?AK/AHȴAG��AF�ABJA=�mA;��A:�A9S�A8��A6�A5��A5
=A3t�A2�A2bA1XA0�A0�uA0E�A0  A/C�A,�HA+ƨA*^5A)A)A)�A*{A*$�A*$�A)��A*z�A+hsA)�;A(v�A(A�A'&�A&5?A%�mA%�hA%%A%%A$�A#�A"�9A"~�A"^5A!��A!�A!��A!��A!t�A!�A �yA VA�A��AhsA�`A��Az�A��A�RAbNAbA��An�A=qAVA=qA$�A(�A  A�-A;dA�A-A�A;dA��A��A�PA�At�AC�A�!A��A�A&�A�AQ�A33A��A��Az�A$�A�A�`A$�A"�A�#AhsA�A
9XA	/A�/A�jA��A��A��A^5AI�A��A|�A"�AA�A�TAdZAXA"�A�A�jAr�A1A�
A��Ap�A ��A 9X@�33@��+@�v�@�V@���@�C�@���@�^5@��@�V@��j@�z�@��;@��@���@�5?@��@���@��m@��@�v�@�h@�r�@� �@�1@���@��@@�=q@�?}@��@�K�@�@�E�@��@�t�@��@�R@�M�@��T@�@��`@�t�@�E�@��/@�1@�t�@���@�E�@��#@ܛ�@ۮ@�9X@�r�@���@���@ܬ@�bN@۾w@�dZ@�;d@�v�@��@�V@�  @�S�@���@�p�@���@Լj@�A�@�dZ@�~�@��@�p�@�&�@�I�@ϥ�@��H@Χ�@�n�@��#@�?}@��@��@�%@���@̛�@�(�@˶F@�dZ@ʸR@�n�@�V@�J@ɲ-@�7L@ȼj@�1'@���@���@ǅ@�"�@��H@�v�@���@�/@��`@Ĵ9@�z�@�b@�l�@��@���@�=q@���@��h@�G�@��@��`@�j@��@��P@�33@�ȴ@��\@�E�@�{@���@��#@�p�@�V@�Ĝ@�j@�Q�@�  @�C�@�ȴ@�ff@��-@�O�@�Ĝ@�I�@���@��@�t�@��y@��\@��@�x�@�%@�Ĝ@�z�@�9X@���@�o@�ȴ@�~�@���@��7@���@���@�1@�\)@��@�ff@�$�@�J@���@���@��/@�I�@��w@�;d@�"�@���@�~�@��#@��@�Z@�  @��;@�;d@�
=@���@���@�n�@�5?@�-@��#@�x�@�X@���@��@�1@���@���@�"�@�n�@�@��#@���@�@��T@���@��@��@��j@��@���@�dZ@���@��h@���@���@���@��@��j@���@���@���@���@�Ĝ@���@�Z@��@��@�K�@�"�@�ȴ@��\@�@���@���@�hs@��/@��u@�Z@���@��@�\)@���@��H@��@���@��!@�~�@��;@�`B@��P@��9@y��@nE�@d�j@[��@R�H@IX@A��@9G�@0Ĝ@)X@"�\@�-@�@1@5?@
�@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	"�B	$�B	$�B	%�B	%�B	%�B	$�B	#�B	#�B	#�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	#�B	#�B	#�B	"�B	�B	hB��B��B��B	1B	JB	bB	�B	;dB	A�B	C�B	B�B	ZB	s�B	�RB
  B
)�B
�B
�yB/BL�B^5By�Br�By�B�\B��B��B��B�PB|�B�oB��B��B�RBB��B��B��B�B�B�5B�B��BȴB�^B��B��B�PBy�Bm�B^5BQ�BF�BD�B8RB5?B.B33B&�BB
��B
��B
x�B
n�B
bNB
Q�B
D�B
33B
�B
B	��B	��B	�B	��B	��B	�#B	�HB	�B	��B	��B	{�B	dZB	T�B	=qB	"�B	VB	B�B�B�B�mB�NB�BB�B��B�
B�5B�)B�B�B�;B�`B�B		7B	�B	"�B	(�B	.B	,B	&�B	%�B	;dB	J�B	M�B	W
B	^5B	dZB	p�B	{�B	��B	�jB	�jB	�qB	��B	ŢB	ƨB	ɺB	ȴB	ɺB	��B	�B	�/B	�;B	�TB	�mB	�mB	�sB	�B	�B	��B
B
%B

=B
JB
DB
JB
VB
bB
bB
bB
\B
\B
\B
\B
JB
JB
bB
hB
uB
�B
�B
�B
�B
�B
�B
{B
uB
bB
\B
\B
VB
VB
PB
VB
hB
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
\B
1B
B
B
B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
JB
JB
PB
PB
JB
DB
DB

=B
+B
B
B
B
B
B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�mB	�fB	�`B	�`B	�fB	�mB	�`B	�NB	�;B	�5B	�5B	�/B	�5B	�;B	�)B	�5B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
  B
  B
  B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
%B
+B
+B
+B
1B
1B
1B
+B
+B
1B
	7B

=B

=B
DB
bB
bB
bB
\B
\B
\B
bB
\B
PB
DB
DB
JB
bB
bB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
,B
0!B
8RB
<jB
D�B
I�B
I�B
N�B
VB
[#B
cTB
gmB
l�B
q�B
v�B
z�B
� B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	"�B	$�B	$�B	%�B	%�B	%�B	$�B	#�B	#�B	#�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	#�B	#�B	#�B	"�B	kB	9B��B��B��B	B	B	4B	�B	;6B	A[B	ChB	BbB	Y�B	s�B	�"B	��B
)�B
��B
�HB.�BL�B^By�Br�By�B�,B�dB�sB�zB�"B|�B�=B��B��B�B�\B��B��B�LB�B�WB� B��BѸBȀB�*B��B�~B�By�Bm`B^BQ�BFsBDhB8B5B-�B2�B&�B �B
ΠB
��B
x�B
nbB
bB
Q�B
DbB
2�B
YB
�B	��B	��B	�jB	��B	ѮB	��B	�B	��B	�MB	�|B	{�B	dB	T�B	=3B	"�B	B	 �B�dB�KB�BB�,B�B��B��BԽB��B��B��B��B��B��B�B�nB	�B	jB	"�B	(�B	-�B	+�B	&�B	%�B	;%B	J~B	M�B	V�B	]�B	dB	pbB	{�B	��B	�'B	�)B	�1B	�DB	�^B	�dB	�vB	�rB	�xB	ԻB	��B	��B	��B	�B	�+B	�)B	�0B	�AB	�PB	�B
�B
�B
	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
1B
HB
jB
cB
]B
UB
EB
7B
1B
B
B
B
B
B

B
B
"B
UB
`B
bB
PB
1B
5B
[B
_B
[B
OB
=B
B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
�B
B
B
B
	B
B

�B

�B
	�B
�B
�B
�B
 �B
�B
 �B	��B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	�mB	�hB	�hB	�eB	�eB	�^B	�JB	�EB	�8B	�9B	�9B	�2B	�3B	�%B	�%B	�B	�B	�B	�!B	�&B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�>B	�WB	�aB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
 �B
 �B
 �B	��B	��B	��B
 �B
 �B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
B
B
B
B
B
B
B
B
B

�B

�B
�B
B
B
B
$B
*B
1B
0B
/B
6B
>B
AB
AB
>B
<B
9B
?B
>B
BB
IB
TB
MB
IB
PB
VB
MB
HB
JB
IB
XB
VB
WB
[B
NB
$�B
+�B
/�B
8B
< B
DQB
IpB
InB
N�B
U�B
Z�B
cB
g B
lBB
q_B
v~B
z�B
�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008022019040510080220190405100802  AO  ARCAADJP                                                                    20181121125959    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125959  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125959  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100802  IP                  G�O�G�O�G�O�                