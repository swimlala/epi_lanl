CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-14T17:02:04Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170814170204  20190405100806  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�V:7j91   @�VβK�@-8���F�d��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  By��B33B���B�  B�  B�  B�  B�33B�33B�33B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�fD�C3D�y�D��3D�	�D�C3D���D�� D�ɚD�L�D�� D�� D� D�9�D�i�D�ɚD�fD�33D�vfD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�=qA	�A)�AI�Ai�A��\A��\A��\A��\Aď\A�A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�BrG�B{�HB��qB��B�#�B�#�B�#�B�#�B�W
B�W
B�W
B��=B��B��B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�W
B�#�B��B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�D0D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Dt�HDy��D��D�UpD���D��pD��D�UpD���D��=D���D�_
D��=D��=D�"=D�K�D�{�D���D��D�EpD�D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A��A��#A��/A��/A��#A���A�ȴAھwA�A�AھwAڕ�AڅA�x�A�p�A�n�A�jA�jA�jA�l�A�n�A�p�A�l�A�l�A�hsA�\)A�9XAمA��A��TA�ƨAҗ�A�XAџ�A�t�A�E�A�  A� �A�bNA���A�\)AʅA���A��A�|�A��
A��AāA�$�A�l�A���A�E�A���A��A��jA�dZA�A�A�A���A��jA���A�A�S�A��`A�bNA�ƨA�7LA���A�dZA��^A�1A���A�5?A�A��yA��-A�A�v�A�I�A��A�&�A��\A���A��7A���A�K�A�K�A�=qA��yA���A�oA��mA�A���A��jA��A���A��
A~I�Ax��Av�Ast�Ap�uAn-AkƨAe�A\��AZ�jAX�DAU"�AT��AShsAP�AN��ALffAJQ�AHbAFZAE33AC�A?�mA=��A<��A:�A9�A8��A8bA7�A5��A4A�A3t�A3/A2�HA1�A0VA.(�A,�\A+K�A*A�A)�#A)%A'�wA&��A&5?A$�A"ȴA�-AVA5?A��A�A�/A��Ar�Ax�A~�A�;A��A�A�+A�wA�wA��AS�A��A�A�A�HA\)AbNA�#A��A�7Ap�A�AE�A�Al�A�A
��A
5?A	oAjA�A�AhsA��A�A�7A�AG�A�7AhsA�A��A�A�TA��A1'A�AC�A��A��A+A �A M�@��;@�J@�O�@��@�r�@�A�@�b@�l�@�v�@�5?@�@���@��/@���@�  @�+@�M�@�n�@��@��y@��@�@�p�@��@���@�Ĝ@��j@� �@�1'@�l�@�9X@�|�@���@�@�^5@�/@�j@�z�@�9X@�Z@�|�@�v�@�{@�`B@���@�A�@� �@���@�dZ@�o@�5?@陚@�/@�r�@��@�+@�\@��@�9@�l�@�ff@�-@�O�@�@�ƨ@�S�@���@�{@�p�@��@���@�Z@��m@ۅ@�t�@�;d@�
=@�v�@��@٩�@ّh@�`B@�%@؛�@�z�@؃@�r�@��;@���@��H@��@��@�&�@�z�@�  @��H@�ff@�-@��@� �@�|�@�K�@��@�+@��H@�-@�`B@�Ĝ@�z�@�(�@� �@���@˝�@�;d@ʸR@��@ɉ7@�V@Ǿw@��@ư!@Ɨ�@�v�@�n�@�=q@��@��@Ł@Ĭ@�A�@öF@�|�@�^5@��`@���@��P@�K�@��R@�E�@�n�@��@�?}@��@�bN@���@�o@��+@��^@�`B@�7L@�/@�V@��@�X@���@�bN@�A�@�l�@��@�+@�"�@���@�M�@���@��h@�hs@�G�@��@�1'@�l�@��\@�~�@�$�@�@���@�hs@��@���@��/@���@�I�@���@��@��m@��P@���@�ff@��@�@��@��7@���@�9X@��m@��w@�;d@��@��!@�5?@�@���@�x�@�p�@�hs@���@�I�@�b@��F@���@�|�@�K�@�@���@���@�5?@�@��@���@���@�Z@�(�@���@��F@��P@�S�@�@���@���@�n�@�V@�-@��@���@���@�`B@��j@���@��u@�(�@��m@���@���@�l�@�S�@�C�@�33@��H@�ff@�{@��T@���@�7L@��D@��@��@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�\)@�S�@�S�@�K�@�o@�ff@�J@��T@��@��`@��D@�Z@�I�@�1'@���@�t�@�\)@�
=@�ff@�=q@���@��@�~�@�@~�@u@l�@ct�@W\)@N�y@F��@>ȴ@8 �@1X@)��@$9X@l�@��@�@^5@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��A��A��#A��/A��/A��#A���A�ȴAھwA�A�AھwAڕ�AڅA�x�A�p�A�n�A�jA�jA�jA�l�A�n�A�p�A�l�A�l�A�hsA�\)A�9XAمA��A��TA�ƨAҗ�A�XAџ�A�t�A�E�A�  A� �A�bNA���A�\)AʅA���A��A�|�A��
A��AāA�$�A�l�A���A�E�A���A��A��jA�dZA�A�A�A���A��jA���A�A�S�A��`A�bNA�ƨA�7LA���A�dZA��^A�1A���A�5?A�A��yA��-A�A�v�A�I�A��A�&�A��\A���A��7A���A�K�A�K�A�=qA��yA���A�oA��mA�A���A��jA��A���A��
A~I�Ax��Av�Ast�Ap�uAn-AkƨAe�A\��AZ�jAX�DAU"�AT��AShsAP�AN��ALffAJQ�AHbAFZAE33AC�A?�mA=��A<��A:�A9�A8��A8bA7�A5��A4A�A3t�A3/A2�HA1�A0VA.(�A,�\A+K�A*A�A)�#A)%A'�wA&��A&5?A$�A"ȴA�-AVA5?A��A�A�/A��Ar�Ax�A~�A�;A��A�A�+A�wA�wA��AS�A��A�A�A�HA\)AbNA�#A��A�7Ap�A�AE�A�Al�A�A
��A
5?A	oAjA�A�AhsA��A�A�7A�AG�A�7AhsA�A��A�A�TA��A1'A�AC�A��A��A+A �A M�@��;@�J@�O�@��@�r�@�A�@�b@�l�@�v�@�5?@�@���@��/@���@�  @�+@�M�@�n�@��@��y@��@�@�p�@��@���@�Ĝ@��j@� �@�1'@�l�@�9X@�|�@���@�@�^5@�/@�j@�z�@�9X@�Z@�|�@�v�@�{@�`B@���@�A�@� �@���@�dZ@�o@�5?@陚@�/@�r�@��@�+@�\@��@�9@�l�@�ff@�-@�O�@�@�ƨ@�S�@���@�{@�p�@��@���@�Z@��m@ۅ@�t�@�;d@�
=@�v�@��@٩�@ّh@�`B@�%@؛�@�z�@؃@�r�@��;@���@��H@��@��@�&�@�z�@�  @��H@�ff@�-@��@� �@�|�@�K�@��@�+@��H@�-@�`B@�Ĝ@�z�@�(�@� �@���@˝�@�;d@ʸR@��@ɉ7@�V@Ǿw@��@ư!@Ɨ�@�v�@�n�@�=q@��@��@Ł@Ĭ@�A�@öF@�|�@�^5@��`@���@��P@�K�@��R@�E�@�n�@��@�?}@��@�bN@���@�o@��+@��^@�`B@�7L@�/@�V@��@�X@���@�bN@�A�@�l�@��@�+@�"�@���@�M�@���@��h@�hs@�G�@��@�1'@�l�@��\@�~�@�$�@�@���@�hs@��@���@��/@���@�I�@���@��@��m@��P@���@�ff@��@�@��@��7@���@�9X@��m@��w@�;d@��@��!@�5?@�@���@�x�@�p�@�hs@���@�I�@�b@��F@���@�|�@�K�@�@���@���@�5?@�@��@���@���@�Z@�(�@���@��F@��P@�S�@�@���@���@�n�@�V@�-@��@���@���@�`B@��j@���@��u@�(�@��m@���@���@�l�@�S�@�C�@�33@��H@�ff@�{@��T@���@�7L@��D@��@��@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�\)@�S�@�S�@�K�@�o@�ff@�J@��T@��@��`@��D@�Z@�I�@�1'@���@�t�@�\)@�
=@�ffG�O�@���@��@�~�@�@~�@u@l�@ct�@W\)@N�y@F��@>ȴ@8 �@1X@)��@$9X@l�@��@�@^5@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�-B	�?B	�?B	�?B	�9B	�9B	�9B	�9B	�9B	�?B	�FB	�FB	�LB	�LB	�LB	�?B	�-B	��B	�PB	��B	��B	�B
	7B
�B
�B
33B
B�B
Q�B
�7B
�\B
��B
��B
�B
�wB
ŢB
ɺB
�B
��BDB�B&�B)�B7LBK�BW
B\)B��B1BJBVB�B)�B-B�B�BĜB�B��B�B�yB  BBB  B  B��B��B�sB�B��B��B��B�1BcTBI�B9XB	7B
�)B
��B
��B
�oB
�B
hsB
D�B
�B	��B	��B	�!B	��B	�+B	�%B	z�B	hsB	^5B	T�B	<jB	6FB	+B	#�B	�B	�B	�B	hB	DB	+B	B	B	  B��B��B��B��B	B	  B	B	B	%B	�B	%�B	33B	6FB	8RB	6FB	6FB	:^B	J�B	Q�B	P�B	O�B	ZB	bNB	hsB	m�B	iyB	k�B	gmB	hsB	l�B	r�B	p�B	p�B	o�B	n�B	l�B	e`B	W
B	J�B	D�B	@�B	:^B	>wB	I�B	R�B	aHB	n�B	r�B	u�B	x�B	n�B	t�B	x�B	y�B	z�B	z�B	z�B	{�B	|�B	{�B	z�B	|�B	�B	�%B	�1B	�7B	�bB	�oB	�bB	�oB	��B	�B	�?B	��B	ɺB	ȴB	ȴB	ǮB	ĜB	�wB	�dB	�XB	�XB	�XB	�dB	�dB	�RB	�XB	�RB	�9B	�9B	�9B	�?B	�9B	�9B	�9B	�RB	�RB	�XB	�XB	�XB	�XB	�qB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�sB	�sB	�fB	�`B	�TB	�NB	�NB	�NB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
1B
+B
+B
1B

=B

=B
DB
DB
JB
JB
PB
VB
\B
bB
hB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
,B
2-B
7LB
?}B
C�B
J�B
P�B
VB
[#B
]/B
`BB
cTB
iyB
k�B
n�B
s�B
w�B
{�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	� B	� B	�B	�B	��B	�&B	�TB	��B	�zB
		B
lB
�B
3B
BbB
Q�B
�	B
�-B
��B
��B
��B
�HB
�pB
ɍB
��B
��BB�B&�B)�B7BK�BV�B[�B��BBB!BjB)�B,�BpB�[B�gB��B��B��B�FB��B�B�B��B��B��B��B�?B��B�RB��B�mB��Bc BI�B9#B	B
��B
̙B
��B
�;B
��B
h?B
DdB
B	��B	ӾB	��B	�uB	��B	��B	z�B	h6B	]�B	T�B	<.B	6
B	*�B	#�B	B	oB	LB	+B	B	�B	�B	�B��B��B��B��B��B	 �B��B	 �B	�B	�B	GB	%�B	2�B	6B	8B	6B	6B	:B	JB	Q�B	P�B	O�B	Y�B	bB	h1B	mNB	i8B	kAB	g+B	h2B	lGB	rlB	p`B	pbB	oYB	nSB	lIB	eB	V�B	JB	DYB	@>B	:B	>1B	IuB	R�B	aB	nUB	rkB	u�B	x�B	nRB	twB	x�B	y�B	z�B	z�B	z�B	{�B	|�B	{�B	z�B	|�B	��B	��B	��B	��B	�B	�(B	�B	�)B	��B	��B	��B	�@B	�vB	�nB	�nB	�jB	�UB	�2B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�,B	�<B	�UB	�iB	͊B	ϗB	ѧB	НB	ϘB	ѦB	ҬB	ҬB	ӰB	տB	��B	��B	�4B	�DB	�:B	�CB	�QB	�DB	�CB	�?B	�<B	�EB	�=B	�8B	�UB	�VB	�WB	�SB	�TB	�IB	�IB	�EB	�8B	�2B	�*B	�'B	�+B	�,B	�B	�B	�
B	�B	�B	�B	� B	��B	�B	�B	�B	�B	�B	� B	�B	�B	�$B	�'B	�1B	�7B	�7B	�6B	�PB	�cB	�gB	�mB	�lB	�uB	�vB	�vB	�rB	�rB	�mB	�iB	�gB	�cB	�VB	�WB	�QB	�;B	�0B	�0B	�(B	�=B	�GB	�HB	�IB	�_B	�hB	�zB	�|B	�zB	�xB	��B	�zB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�sB	�hB	�fB	�gB	�lB	��B	��B	��B	��B	�zB	��B	�B	�yB	�mB	�eB	�`B	�gB	�fB	�fB	�mB	�B	��B	��B	��B	��B	��B	��B	�~B	�B	�qB	��B	�}B	�{B	�zB	�qB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B
 B
B
B
B
B
B
B
'B
#B
#B
&B
+B
2B
2B
1B
1B
1B
2B
1B
2B
1B
2B
3B
4B
*B
/B
1B
1B
1B
8B
8B
<B
<B
7B
;B
;B
=B
=B
JG�O�B
\B
"�B
+�B
1�B
7B
?5B
CJB
JvB
P�B
U�B
Z�B
\�B
_�B
c
B
i-B
k9B
nNB
smB
w�B
{�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008062019040510080620190405100806  AO  ARCAADJP                                                                    20170814170204    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170814170204  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170814170204  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100806  IP                  G�O�G�O�G�O�                