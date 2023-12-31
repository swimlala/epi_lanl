CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-01-11T13:00:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200111130027  20200111130027  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @��Y��9^1   @��\�n�@.�Q��d|���S�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D��D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�=qA	�A)�AI�Aj�RA��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�BrG�BzG�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B��B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{DhDh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Du${Du�{Dv${Dv�{Dw${Dw�{Dx${Dx�{Dy${Dy�{Dz${Dz�{D{${D{�{D|${D|�{D}${D}�{D~${D~�{D${D�{D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��pD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��pD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��
D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�O
D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D=D��=D�=D�R=DÒ=D��=D�=D�R=DĒ=D��=D�=D�R=DŒ=D��=D�=D�R=Dƒ=D��=D�=D�R=Dǒ=D��=D�=D�R=DȒ=D��=D�=D�R=Dɒ=D��=D�=D�R=Dʒ=D��=D�=D�R=D˒=D��=D�=D�R=D̒=D��=D�=D�R=D͒=D��=D�=D�R=DΒ=D��=D�=D�R=Dϒ=D��=D�=D�R=DВ=D��=D�=D�R=Dђ=D��=D�=D�R=DҒ=D��=D�=D�R=DӒ=D��=D�=D�R=DԒ=D��=D�=D�R=DՒ=D��=D�=D�R=D֒=D��=D�=D�R=Dג=D��=D�=D�R=Dؒ=D��=D�=D�R=Dْ=D��=D�=D�R=Dڒ=D��=D�=D�R=Dے=D��=D�=D�R=Dܒ=D��=D�=D�R=Dݒ=D��=D�=D�R=Dޒ=D��=D�=D�R=Dߒ=D��=D�=D�R=D��=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�UpD�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�
D�R=D�=D��
D�
D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D��pD��pD�p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AՕ�Aա�A՗�Aա�Aե�Aե�Aգ�Aէ�Aէ�Aե�Aթ�Aէ�Aէ�AՋDA�t�A�z�A�v�A�jA�`BA�VA�M�A�G�A�C�A�;dA�9XA�5?A�1'A�+A�&�A��A��A�oA�VA�A��A��HA���Aԡ�A�O�A���A�r�A�7LA�XA�/A¸RA��A��PA���A��mA�  A�VA�hsA���A��PA��RA��A��A�M�A���A��A�dZA�Q�A�  A�x�A�  A���A���A�VA�
=A�n�A��/A�M�A��A�ffA���A�-A��mA���A��A��RA�I�A�I�A33A{�Au�#As�-AnjAkx�Ai�AhffAf9XAb(�A`��A^~�AY�wAU\)AQVAPjAOx�AL��AKG�AI�AG�^ADA�A@A�A>�9A<�RA9��A9K�A8�A7��A4ĜA3p�A29XA1S�A1�-A2{A1�FA/�A/"�A0ZA0�uA2bA1��A1`BA0�+A/��A/
=A.��A.{A-C�A,ZA+��A+x�A*��A*(�A)�FA*�A*(�A)�A(�yA'��A'x�A'&�A&��A%�A%�
A%�A$VA#p�A"�A"z�A!�FA ��A�wA�`A9XA��At�A��AM�A�A�DA�AƨA�^A��A&�AI�A�mA�-A7LA=qA33A��A�An�A�
AE�Ar�AZAffAZAv�A�!A�DAbNA  Ap�A�yA��A�+A�A7LAoA�HAQ�A��A�A�DA�;A+A
�\A
VA	��A	/A	oA��A�A=qAJA�AdZAA��A�A��A�uAffA�A�A�AS�A�/An�A{AƨA��A��Ap�AO�A&�AĜA�A��A��A33A ��A Z@�\)@�$�@��T@��^@��@���@�A�@�1@��;@���@�o@��^@�?}@��@�1'@�dZ@���@�V@�X@���@��@��@�dZ@�o@���@�@�5?@��@�I�@��@�dZ@�C�@�"�@��@�v�@��-@�7L@�j@�1@��@ꗍ@�-@�@�hs@�&�@��@�ff@��@�%@�j@�z�@�@�(�@㝲@�33@�ff@��T@�hs@���@�j@���@���@��#@�Z@��
@�;d@�E�@�{@٩�@�p�@�V@��`@؛�@�A�@���@�o@և+@�5?@�@�x�@�r�@�9X@�1'@� �@���@�ƨ@ӶF@�S�@�@ҸR@҇+@�E�@щ7@�&�@���@��`@�z�@���@���@�\)@�
=@�ff@��@͑h@�7L@̼j@˅@�v�@�M�@�=q@�$�@�J@��#@ɲ-@ɑh@���@ȃ@�1@��
@ǍP@�\)@�;d@�"�@��@�n�@�5?@ř�@�7L@��`@ēu@�A�@��m@�|�@�\)@�K�@��@�@�ȴ@��@���@�V@��@�  @��w@�t�@���@���@�M�@�M�@�-@���@��7@�/@��`@��D@�1@�33@��H@�ȴ@��!@�E�@���@�p�@���@��j@�(�@��;@�+@�ȴ@�V@��@���@���@���@��@�&�@���@�I�@�  @��;@��w@��@�M�@�J@��@��^@�?}@���@�I�@��P@��@��y@��!@���@���@��\@�^5@�@��^@��h@�O�@��@��j@��u@�I�@�b@�|�@���@��\@�$�@��^@�X@��/@��9@�z�@��@��w@�+@�ff@�J@�@���@��m@���@��@�ȴ@���@��R@��R@���@���@��+@��@��#@�@���@��@��@���@��h@�X@�G�@��@��@�z�@��@���@���@�\)@�@���@�5?@�{@��@�@�hs@�7L@���@���@�j@�A�@�(�@��@�  @��;@���@��F@��@�\)@��@���@��@���@���@�n�@�5?@�$�@���@��-@��7@�X@�7L@�V@��`@��@�z�@�A�@�1@��@���@��H@��R@�v�@�E�@�=q@�$�@���@��T@��^@��@�A�@��w@���@�|�@�C�@���@�^5@�E�@�J@���@��@��9@��@��@�I�@�(�@�  @���@��@�ƨ@���@�\)@�+@���@�ȴ@���@�~�@�-@��T@���@��@�`B@���@��`@���@��@�j@���@���@�|�@�@�M�@��@�J@��@�x�@��`@��@�Q�@�  @��@��@��R@���@���@�~�@�E�@�-@��@���@��@�7L@�V@�V@�V@�%@�Ĝ@��@�bN@�9X@��m@���@��F@��@�K�@�"�@�v�@�$�@�@���@�x�@�/@��`@��@��D@� �@\)@~�R@~ff@~5?@~@}�T@}��@}`B@|��@|�j@|�D@|Z@{�m@{�@z��@zM�@z�@zJ@zJ@y��@y&�@x�@x �@w�;@w��@w��@w;d@v�@vv�@vE�@u��@t�/@s��@so@r�!@r=q@q��@q�7@qx�@q%@p  @o\)@o\)@oK�@o;d@o+@n�@n5?@m@m�h@l��@l�D@k��@k��@k"�@j�!@jn�@j=q@jJ@i�7@iG�@i�@hQ�@g��@g+@fȴ@e�T@e@e��@e?}@d�@d�D@dj@d(�@c�@c"�@b��@b~�@bM�@a��@a&�@`Ĝ@`��@`��@`�u@`�@`b@_l�@_�@_
=@^��@^��@^�R@^E�@]�@]?}@\I�@[�@[33@Z^5@ZM�@ZM�@ZM�@ZM�@Z=q@Y��@Y��@Y%@X�9@X1'@W�w@W��@W�P@WK�@W+@V��@V�R@VV@U�T@U�-@T��@T�@Tj@S�
@SS�@S"�@R~�@R-@RJ@Q��@QX@Q7L@Q�@PĜ@O��@Nȴ@NE�@M�@M@M�h@M/@L��@Lz�@L9X@K��@KC�@J�H@J^5@J�@I��@I�@I��@IX@H�`@H��@H��@Gl�@G�@Fȴ@E�@E@E�-@Ep�@E�@D�@D�@D�D@DZ@C��@C��@CC�@Co@B��@BM�@AX@@��@@��@@1'@?�@?�;@?�w@?l�@>��@>@=�@=�@=�@=V@<�@<�D@<z�@<(�@;�@;"�@:��@:-@9�^@9x�@97L@8��@8A�@8A�@81'@8  @7�w@7l�@6��@6�@6��@6��@6ȴ@6�+@6$�@5��@5@5�@4�@3ƨ@3��@3dZ@3S�@333@3"�@3@2�@2�!@2��@2~�@2^5@2=q@1��@1��@1X@1%@0��@0�@0bN@01'@/�@/�P@/;d@/�@.�y@.�+@.ff@.V@.E�@.$�@-�@-��@-@-�-@-�@,�j@,j@,9X@,(�@,�@+�m@+�@+C�@*�@*n�@*M�@*M�@*M�@*M�@*-@*J@)�^@)�7@)hs@)7L@)%@(��@(��@(r�@(A�@'�@'|�@'\)@'\)@'K�@&�y@&�R@&ff@&$�@&{@&{@%�@%�-@%�h@%`B@%O�@%/@%�@$��@$9X@$1@#�m@#��@#t�@#C�@#o@"��@"�!@"��@"=q@!��@!�7@!X@!�@!%@!%@!%@!%@!%@!%@ ��@ �`@ �9@ �u@ r�@ 1'@��@�@��@�P@;d@�R@�+@ff@E�@5?@$�@{@�@�-@p�@?}@�@�@V@��@�@�@�/@��@�j@�j@�@��@z�@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՕ�Aա�A՗�Aա�Aե�Aե�Aգ�Aէ�Aէ�Aե�Aթ�Aէ�Aէ�AՋDA�t�A�z�A�v�A�jA�`BA�VA�M�A�G�A�C�A�;dA�9XA�5?A�1'A�+A�&�A��A��A�oA�VA�A��A��HA���Aԡ�A�O�A���A�r�A�7LA�XA�/A¸RA��A��PA���A��mA�  A�VA�hsA���A��PA��RA��A��A�M�A���A��A�dZA�Q�A�  A�x�A�  A���A���A�VA�
=A�n�A��/A�M�A��A�ffA���A�-A��mA���A��A��RA�I�A�I�A33A{�Au�#As�-AnjAkx�Ai�AhffAf9XAb(�A`��A^~�AY�wAU\)AQVAPjAOx�AL��AKG�AI�AG�^ADA�A@A�A>�9A<�RA9��A9K�A8�A7��A4ĜA3p�A29XA1S�A1�-A2{A1�FA/�A/"�A0ZA0�uA2bA1��A1`BA0�+A/��A/
=A.��A.{A-C�A,ZA+��A+x�A*��A*(�A)�FA*�A*(�A)�A(�yA'��A'x�A'&�A&��A%�A%�
A%�A$VA#p�A"�A"z�A!�FA ��A�wA�`A9XA��At�A��AM�A�A�DA�AƨA�^A��A&�AI�A�mA�-A7LA=qA33A��A�An�A�
AE�Ar�AZAffAZAv�A�!A�DAbNA  Ap�A�yA��A�+A�A7LAoA�HAQ�A��A�A�DA�;A+A
�\A
VA	��A	/A	oA��A�A=qAJA�AdZAA��A�A��A�uAffA�A�A�AS�A�/An�A{AƨA��A��Ap�AO�A&�AĜA�A��A��A33A ��A Z@�\)@�$�@��T@��^@��@���@�A�@�1@��;@���@�o@��^@�?}@��@�1'@�dZ@���@�V@�X@���@��@��@�dZ@�o@���@�@�5?@��@�I�@��@�dZ@�C�@�"�@��@�v�@��-@�7L@�j@�1@��@ꗍ@�-@�@�hs@�&�@��@�ff@��@�%@�j@�z�@�@�(�@㝲@�33@�ff@��T@�hs@���@�j@���@���@��#@�Z@��
@�;d@�E�@�{@٩�@�p�@�V@��`@؛�@�A�@���@�o@և+@�5?@�@�x�@�r�@�9X@�1'@� �@���@�ƨ@ӶF@�S�@�@ҸR@҇+@�E�@щ7@�&�@���@��`@�z�@���@���@�\)@�
=@�ff@��@͑h@�7L@̼j@˅@�v�@�M�@�=q@�$�@�J@��#@ɲ-@ɑh@���@ȃ@�1@��
@ǍP@�\)@�;d@�"�@��@�n�@�5?@ř�@�7L@��`@ēu@�A�@��m@�|�@�\)@�K�@��@�@�ȴ@��@���@�V@��@�  @��w@�t�@���@���@�M�@�M�@�-@���@��7@�/@��`@��D@�1@�33@��H@�ȴ@��!@�E�@���@�p�@���@��j@�(�@��;@�+@�ȴ@�V@��@���@���@���@��@�&�@���@�I�@�  @��;@��w@��@�M�@�J@��@��^@�?}@���@�I�@��P@��@��y@��!@���@���@��\@�^5@�@��^@��h@�O�@��@��j@��u@�I�@�b@�|�@���@��\@�$�@��^@�X@��/@��9@�z�@��@��w@�+@�ff@�J@�@���@��m@���@��@�ȴ@���@��R@��R@���@���@��+@��@��#@�@���@��@��@���@��h@�X@�G�@��@��@�z�@��@���@���@�\)@�@���@�5?@�{@��@�@�hs@�7L@���@���@�j@�A�@�(�@��@�  @��;@���@��F@��@�\)@��@���@��@���@���@�n�@�5?@�$�@���@��-@��7@�X@�7L@�V@��`@��@�z�@�A�@�1@��@���@��H@��R@�v�@�E�@�=q@�$�@���@��T@��^@��@�A�@��w@���@�|�@�C�@���@�^5@�E�@�J@���@��@��9@��@��@�I�@�(�@�  @���@��@�ƨ@���@�\)@�+@���@�ȴ@���@�~�@�-@��T@���@��@�`B@���@��`@���@��@�j@���@���@�|�@�@�M�@��@�J@��@�x�@��`@��@�Q�@�  @��@��@��R@���@���@�~�@�E�@�-@��@���@��@�7L@�V@�V@�V@�%@�Ĝ@��@�bN@�9X@��m@���@��F@��@�K�@�"�@�v�@�$�@�@���@�x�@�/@��`@��@��D@� �@\)@~�R@~ff@~5?@~@}�T@}��@}`B@|��@|�j@|�D@|Z@{�m@{�@z��@zM�@z�@zJ@zJ@y��@y&�@x�@x �@w�;@w��@w��@w;d@v�@vv�@vE�@u��@t�/@s��@so@r�!@r=q@q��@q�7@qx�@q%@p  @o\)@o\)@oK�@o;d@o+@n�@n5?@m@m�h@l��@l�D@k��@k��@k"�@j�!@jn�@j=q@jJ@i�7@iG�@i�@hQ�@g��@g+@fȴ@e�T@e@e��@e?}@d�@d�D@dj@d(�@c�@c"�@b��@b~�@bM�@a��@a&�@`Ĝ@`��@`��@`�u@`�@`b@_l�@_�@_
=@^��@^��@^�R@^E�@]�@]?}@\I�@[�@[33@Z^5@ZM�@ZM�@ZM�@ZM�@Z=q@Y��@Y��@Y%@X�9@X1'@W�w@W��@W�P@WK�@W+@V��@V�R@VV@U�T@U�-@T��@T�@Tj@S�
@SS�@S"�@R~�@R-@RJ@Q��@QX@Q7L@Q�@PĜ@O��@Nȴ@NE�@M�@M@M�h@M/@L��@Lz�@L9X@K��@KC�@J�H@J^5@J�@I��@I�@I��@IX@H�`@H��@H��@Gl�@G�@Fȴ@E�@E@E�-@Ep�@E�@D�@D�@D�D@DZ@C��@C��@CC�@Co@B��@BM�@AX@@��@@��@@1'@?�@?�;@?�w@?l�@>��@>@=�@=�@=�@=V@<�@<�D@<z�@<(�@;�@;"�@:��@:-@9�^@9x�@97L@8��@8A�@8A�@81'@8  @7�w@7l�@6��@6�@6��@6��@6ȴ@6�+@6$�@5��@5@5�@4�@3ƨ@3��@3dZ@3S�@333@3"�@3@2�@2�!@2��@2~�@2^5@2=q@1��@1��@1X@1%@0��@0�@0bN@01'@/�@/�P@/;d@/�@.�y@.�+@.ff@.V@.E�@.$�@-�@-��@-@-�-@-�@,�j@,j@,9X@,(�@,�@+�m@+�@+C�@*�@*n�@*M�@*M�@*M�@*M�@*-@*J@)�^@)�7@)hs@)7L@)%@(��@(��@(r�@(A�@'�@'|�@'\)@'\)@'K�@&�y@&�R@&ff@&$�@&{@&{@%�@%�-@%�h@%`B@%O�@%/@%�@$��@$9X@$1@#�m@#��@#t�@#C�@#o@"��@"�!@"��@"=q@!��@!�7@!X@!�@!%@!%@!%@!%@!%@!%@ ��@ �`@ �9@ �u@ r�@ 1'@��@�@��@�P@;d@�R@�+@ff@E�@5?@$�@{@�@�-@p�@?}@�@�@V@��@�@�@�/@��@�j@�j@�@��@z�@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�?B�9B�?B�9B�9B�?B�?B�9B�9B�9B�9B�9B�9B�XB�jB�}B��BBÖBÖBĜBŢBŢBƨBƨBǮBȴBɺB��B��B��B��B��B�B�;B�sB�B	+B
1'B
�yBk�B�=B�hB�!B�HB�mB��BɺB��B��B�B��B��B��B��B��B��B��B��B��B�?B�B��B�%B� BffBXBF�B,BVBB
�B
�}B
�RB
��B
�+B
r�B
ffB
]/B
VB
H�B
0!B
�B
B	�TB	�B	�dB	��B	��B	�{B	�B	p�B	gmB	XB	C�B	-B	�B	�B	VB	B��B�B�B�BB�;B�B�yB�HB�HB�5B�B�/B��B	+B	+B	<jB	K�B	_;B	gmB	�B	��B	�qB	��B

=B
uB
�B
#�B
&�B
)�B
/B
49B
8RB
8RB
8RB
8RB
9XB
A�B
I�B
M�B
VB
[#B
_;B
aHB
bNB
e`B
cTB
k�B
o�B
k�B
k�B
l�B
l�B
o�B
n�B
gmB
_;B
ZB
[#B
XB
XB
W
B
R�B
K�B
F�B
E�B
D�B
B�B
?}B
:^B
7LB
6FB
49B
6FB
5?B
49B
49B
49B
1'B
+B
7LB
9XB
>wB
C�B
M�B
VB
W
B
W
B
W
B
W
B
VB
VB
VB
W
B
W
B
W
B
VB
VB
VB
T�B
W
B
T�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
R�B
R�B
S�B
S�B
R�B
R�B
R�B
R�B
R�B
Q�B
P�B
O�B
N�B
M�B
L�B
L�B
K�B
J�B
K�B
K�B
J�B
J�B
J�B
I�B
G�B
G�B
F�B
E�B
D�B
C�B
B�B
B�B
B�B
B�B
B�B
A�B
A�B
A�B
@�B
@�B
?}B
=qB
<jB
<jB
;dB
:^B
9XB
8RB
8RB
7LB
7LB
7LB
7LB
6FB
5?B
5?B
49B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
5?B
49B
33B
33B
2-B
1'B
1'B
0!B
0!B
0!B
/B
)�B
$�B
#�B
"�B
"�B
%�B
)�B
(�B
'�B
&�B
%�B
$�B
#�B
%�B
%�B
%�B
%�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
&�B
&�B
&�B
'�B
'�B
(�B
'�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
,B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
33B
33B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
:^B
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
=qB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
>wB
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�2B�>B�DB�KB�KB�QB�WB�WB�]B�]B�cB�iB�oB�vB�|B��B��B§B��B��B�(B�YB��B
 �B
�.B[:By�B�B��B��B�"B��B�oB��BíBŹB��B§B��B��BíB§B��B§B�vB��B��B�<Bu�Bo�BVBG�B6]B�B
�B
�B
��B
�2B
�B
��B
v�B
beB
VB
L�B
E�B
8iB
�B
mB	��B	�	B	ŹB	�B	��B	�mB	�0B	r�B	`YB	W"B	G�B	3KB	�B	
UB	6B�B��B�B�eB�@B��B��B�FB�.B��B��B��B��B��B�B��B	�B	,B	;|B	N�B	W"B	q�B	��B	�&B	�B	��B
*B
aB
�B
�B
�B
�B
#�B
(B
(B
(B
(B
)B
1>B
9oB
=�B
E�B
J�B
N�B
P�B
RB
UB
S	B
[:B
_SB
[:B
[:B
\@B
\@B
_SB
^MB
W"B
N�B
I�B
J�B
G�B
G�B
F�B
B�B
;|B
6]B
5WB
4QB
2DB
/2B
*B
'B
%�B
#�B
%�B
$�B
#�B
#�B
#�B
 �B
�B
'B
)B
.,B
3KB
=�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
F�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
C�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
?�B
>�B
=�B
<�B
<�B
;|B
:vB
;|B
;|B
:vB
:vB
:vB
9oB
7cB
7cB
6]B
5WB
4QB
3KB
2DB
2DB
2DB
2DB
2DB
1>B
1>B
1>B
08B
08B
/2B
-&B
,B
,B
+B
*B
)B
(B
(B
'B
'B
'B
'B
%�B
$�B
$�B
#�B
$�B
$�B
%�B
$�B
$�B
$�B
$�B
$�B
#�B
"�B
"�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
sB
gB
aB
[B
[B
[B
[B
[B
[B

UB

UB
aB
aB
aB
aB
[B

UB
<B
<B
HB

UB

UB

UB

UB

UB
aB
gB
aB
aB
[B

UB

UB

UB
[B
[B
[B
[B
[B

UB

UB
	OB
	OB
HB
HB
BB
BB
BB
BB
<B
<B
<B
<B
BB
BB
BB
BB
HB
HB
HB
HB
	OB

UB
	OB
HB
BB
<B
<B
BB
BB
<B
<B
<B
HB
HB
HB
HB
HB

UB
	OB
	OB
	OB
	OB
	OB

UB

UB

UB
	OB
	OB
	OB
	OB

UB

UB

UB

UB
[B
[B

UB

UB

UB

UB

UB

UB

UB

UB
[B
[B
[B
[B
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
aB
gB
gB
mB
mB
mB
mB
mB
mB
mB
mB
sB
sB
sB
sB
sB
zB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
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
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
'B
'B
'B
'B
(B
)B
)B
)B
)B
)B
)B
)B
)B
)B
)B
)B
*B
+B
+B
+B
*B
,B
,B
,B
,B
-&B
.,B
.,B
.,B
-&B
,B
,B
-&B
.,B
.,B
/2B
/2B
.,B
/2B
/2B
/2B
08B
1>B
1>B
1>B
1>B
1>B
2DB
2DB
2DB
3KB
3KB
3KB
2DB
3KB
3KB
4QB
4QB
4QB
4QB
5WB
6]B
6]B
7cB
6]B
7cB
7cB
8iB
8iB
8iB
8iB
8iB
8iB
9oB
9oB
9oB
9oB
9oB
:vB
9oB
9oB
9oB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
;|B
;|B
;|B
;|B
<�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
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
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
RB
RB
RB
S	B
S	B
S	B
S	B
S	B
S	B
TB
TB
S	B
TB
UB
UB
UB
VB
UB
VB
VB
VB
VB
VB
VB
W"B
W"B
W"B
W"B
W"B
W"B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
^MB
^MB
^MB
^MB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
_SB
_SB
`YB
`YB
`YB
_SB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
a_B
beB
beB
beB
beB
beB
ckB
ckB
dqB
dqB
dqB
dqB
dqB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.57 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200111130027              20200111130027  AO  ARCAADJP                                                                    20200111130027    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200111130027    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200111130027  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200111130027  QCF$                G�O�G�O�G�O�0               