CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-05-29T10:01:20Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220529100120  20220529100120  4903322 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               NA   AO  8286                            2B  A   NAVIS_A                         1165                            170425                          863 @�ә�7k�1   @�ӚUUbF@8��G�{�d����m1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         NA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CC��CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv.Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D/��D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��\D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D��\D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D�D�D��D�B�DÂ�D�D��D�B�DĂ�D�D��D�B�Dł�D�D��D�B�DƂ�D�D��D�B�Dǂ�D�D��D�B�DȂ�D�D��D�B�Dɂ�D�D��D�B�Dʂ�D�D��D�B�D˂�D�D��D�B�D̂�D�D��D�B�D͂�D�D��D�B�D΂�D�D��D�B�Dς�D�D��D�B�DЂ�D�D��D�B�Dт�D�D��D�B�D҂�D�D��D�B�Dӂ�D�D��D�B�DԂ�D�D��D�B�DՂ�D�D��D�B�Dւ�D�D��D�B�Dׂ�D�D��D�B�D؂�D�D��D�B�Dق�D�D��D�B�Dڂ�D�D��D�B�Dۂ�D�D��D�B�D܂�D�D��D�B�D݂�D�D��D�B�Dނ�D�D��D�B�D߂�D�D��D�B�D���D�D��D�B�DႏD�D��D�B�D₏D�D��D�B�DわD�D��D�B�D䂏D�D��D�B�D傏D�D��D�B�D悏D�D��D�B�D炏D�D��D�B�D肏D�\D��D�B�D邏D�D��D�B�DꂏD�D��D�B�D낏D�D��D�B�D삏D�D��D�B�D킏D�D��D�B�DD�D��D�B�DD�D��D�B�D���D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D�D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�B�D���D�D��D�E�D�o\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���Aϣ�A��`A��A�bNA��A�r�A�{A���A�n�A���A�x�A�;dA��A��#A�&�A��\A��A�bNA�G�A���A�$�A��!A��+A���A��PA�p�A�C�A��A���A���A�r�A�bA��A��A�jA��/A��A�ĜA�r�A�A�A���A��A�M�A��A���A���A��DA�K�A���A��`A��#A��A��jA��yA���A�=qA�XA��+A���A��uA���A�ffA���A�$�A��-A�"�A�VA���A��#A���A�I�A�oA��A���A���A�A�
=A�VA���A�r�A�jA�VA�O�A��FA�-A���A���A�ZA�n�A�|�A��A�1A��
A��A��9A�~�A�M�A���A�ffA��+A��HA�jA�ZA���AA}ƨA{�Az�!Az�Ax��Aw��Av�/At^5Ar�Ap��An5?Al�Aj�\Ai��Ai;dAh��AhQ�Agx�Af1'AeG�Ac�AbA�A]�7AZM�AY�AX�9AXffAW�^AV�jAU+ATbASp�AS33AR��ARZAQ�AP��AN�AM+AJ�`AIVAG�AF�AF�\AFr�AE33ADM�ACG�ABI�AAS�A?S�A>�A<��A;�hA:�`A:$�A8��A7�;A6��A5��A5�7A4��A4bA3�^A3hsA2v�A1��A1�PA0��A0bNA0$�A/A/\)A.�DA.{A-l�A,v�A+�wA*��A)��A'�A'�A&�A%��A$�9A$^5A#��A#7LA"�A" �A!S�A �\A n�A�AA�A��A�A��A��A=qAAJA��A�Az�AĜA;dAv�AI�A=qA$�AA��AA�jA�^A
~�A�HAjA^5AA�A-AJA�Ax�A(�A�A�A"�Ao@�t�@�p�@��/@���@�ȴ@��`@��-@�-@�E�@�!@��@��@�@�+@�^@���@��@��/@�j@�z�@�|�@�;d@�;d@�ȴ@�O�@�1@��@�v�@�{@؛�@��@��
@�|�@�C�@�o@�V@�33@�hs@���@�j@�+@�^5@��@�x�@̴9@���@ʇ+@�{@Ɂ@�j@�Z@���@�v�@�@�Ĝ@��m@���@�$�@��@��T@��7@��`@� �@��;@�K�@�+@���@�{@���@�G�@���@�Q�@�C�@�+@��@�x�@�&�@�&�@�j@�ƨ@�K�@���@�v�@� �@�X@�9X@���@��\@��#@�&�@��m@��@�dZ@�ff@��-@���@�1@��
@���@��@��R@���@�v�@�@��@��
@�C�@��R@�5?@���@��w@�t�@�S�@��@���@�{@�p�@���@�z�@�Z@���@�t�@�"�@���@�~�@�E�@�-@�-@��@��T@�`B@�V@��@�Ĝ@���@�A�@�1@��m@��w@��@��+@��@���@��h@�G�@��`@��j@���@��u@��D@��@��w@�dZ@�;d@�
=@��@���@���@��@��#@�x�@�?}@��/@� �@���@���@��@��;@���@�t�@�\)@�33@��H@���@���@�ȴ@���@��+@�~�@�n�@�M�@��@�@�X@��@��@��@�Q�@�1'@��@�  @���@��@��@�|�@�C�@���@�5?@�5?@��7@��@���@�Ĝ@��9@��@�Q�@��w@��@��@�+@�o@���@���@�V@�@�@��@���@���@�`B@�7L@�/@�&�@�%@��9@��@�r�@�Q�@�A�@�(�@�;@�P@~�y@~{@}O�@|��@|�D@|I�@{ƨ@{t�@{S�@{33@{"�@{@z�@z��@zJ@y�7@yX@xĜ@xr�@xbN@xQ�@x1'@x  @w\)@v��@v��@vV@v$�@u�@u@up�@uO�@u�@uV@t��@t��@t�@s@r��@r��@r�!@r��@r�\@rn�@rM�@rJ@q��@q7L@p�9@pr�@pbN@pbN@pA�@pb@o�@o�w@o;d@n�R@m�@m�-@m�@lz�@k��@kC�@jM�@j�@i�7@h�`@hr�@h �@f��@fV@f{@e�@e@e�@e?}@d�/@d9X@d�@cƨ@c��@c��@c��@c��@cC�@b��@a��@ahs@`��@`r�@_�w@_��@_�P@_l�@_K�@_+@^�y@^E�@^{@]@]�h@]`B@\�@\z�@[t�@Z��@Z��@Z^5@Y��@Y��@YX@Y�@X��@Xr�@XA�@W�@W�@W\)@V��@V��@V5?@UO�@T��@T�j@T�D@T�D@TZ@S�
@SC�@So@So@So@S@R�H@R��@Rn�@Q�@Q�#@Q��@Qhs@Q7L@Q&�@Q�@P��@PĜ@P��@P�u@P�u@P�@PA�@Pb@P  @O�w@O;d@Nff@N{@M�@M�@M?}@L��@L�@L�/@L�j@L��@LZ@LI�@L1@K�@KC�@J�@J�\@JM�@I��@I�^@I��@I��@I7L@H��@H�u@HbN@H1'@Hb@H  @G�w@G�P@GK�@F�R@F@E�h@E�@Dz�@C�m@Co@B��@A�@A7L@A%@@�`@@�9@@bN@?��@?�w@?�w@?�P@?l�@?\)@>��@>ff@>E�@>@=��@<��@<j@<(�@;t�@;"�@;o@;@:��@:=q@9��@9x�@9&�@8��@8�@8bN@8 �@7��@7K�@7�@7
=@6��@6$�@5�T@5�T@5��@5p�@5V@4�@4�/@4�@4�D@49X@4�@3�F@3�@3S�@333@3o@2�H@2�!@2~�@2M�@2J@1�@1��@1��@1hs@1G�@1&�@0�`@0��@0�9@0��@0�@0Q�@01'@/�;@/�P@/+@/
=@.ȴ@.��@.��@.�+@.V@-@-?}@,��@,�j@,Z@,9X@,9X@+�
@+C�@+"�@+@*�@*��@*�!@)��@)x�@)%@(�u@(bN@'�@'l�@'K�@'K�@'K�@'\)@'K�@&�R@&V@&E�@&@%@%��@%`B@$��@$�/@$��@$��@$��@$z�@$I�@$�@$�@$�@$�@$�@#�m@#t�@#o@"��@"�\@!�@!x�@!7L@!%@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ bN@�w@|�@\)@K�@;d@+@��@�R@v�@V@5?@{@`B@?}@?}@?}@V@�@Z@(�@�@�@�@1@�m@�m@ƨ@ƨ@ƨ@�F@�F@�F@t�@�@�H@��@M�@�@�7@hs@X@�@�9@�@r�@r�@bN@A�@  @�w@;d@�+@ff@ff@V@$�@�-@�@�/@�j@�@�@��@��@�D@z�@(�@ƨ@��@dZ@o@�H@��@��@~�@=q@J@�^@�7@X@&�@%@%@��@��@�9@�@r�@1'@�;@�P@;d@
=@ȴ@�+@v�@ff@ff@ff@V@E�@E�@E�@$�@@@`B@/@�j@��@Z@�@��@�
@�F@�@t�@"�@
�!@
�\@
~�@
M�@
=q@
�@	��@	��@	��@	��@	hs@	G�@	7L@	�@�`@��@Ĝ@�u@1'@b@  @  @�@�;@��@��@�@�P@l�@;d@+@+@�@�@��@ȴ@��@�+@v�@v�@ff@V@$�@@�@��@�@`B@?}@�@��@��@�@�D@z�@1@��@S�@C�@33@"�@@��@��@�\@^5@M�@=q@-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���Aϣ�A��`A��A�bNA��A�r�A�{A���A�n�A���A�x�A�;dA��A��#A�&�A��\A��A�bNA�G�A���A�$�A��!A��+A���A��PA�p�A�C�A��A���A���A�r�A�bA��A��A�jA��/A��A�ĜA�r�A�A�A���A��A�M�A��A���A���A��DA�K�A���A��`A��#A��A��jA��yA���A�=qA�XA��+A���A��uA���A�ffA���A�$�A��-A�"�A�VA���A��#A���A�I�A�oA��A���A���A�A�
=A�VA���A�r�A�jA�VA�O�A��FA�-A���A���A�ZA�n�A�|�A��A�1A��
A��A��9A�~�A�M�A���A�ffA��+A��HA�jA�ZA���AA}ƨA{�Az�!Az�Ax��Aw��Av�/At^5Ar�Ap��An5?Al�Aj�\Ai��Ai;dAh��AhQ�Agx�Af1'AeG�Ac�AbA�A]�7AZM�AY�AX�9AXffAW�^AV�jAU+ATbASp�AS33AR��ARZAQ�AP��AN�AM+AJ�`AIVAG�AF�AF�\AFr�AE33ADM�ACG�ABI�AAS�A?S�A>�A<��A;�hA:�`A:$�A8��A7�;A6��A5��A5�7A4��A4bA3�^A3hsA2v�A1��A1�PA0��A0bNA0$�A/A/\)A.�DA.{A-l�A,v�A+�wA*��A)��A'�A'�A&�A%��A$�9A$^5A#��A#7LA"�A" �A!S�A �\A n�A�AA�A��A�A��A��A=qAAJA��A�Az�AĜA;dAv�AI�A=qA$�AA��AA�jA�^A
~�A�HAjA^5AA�A-AJA�Ax�A(�A�A�A"�Ao@�t�@�p�@��/@���@�ȴ@��`@��-@�-@�E�@�!@��@��@�@�+@�^@���@��@��/@�j@�z�@�|�@�;d@�;d@�ȴ@�O�@�1@��@�v�@�{@؛�@��@��
@�|�@�C�@�o@�V@�33@�hs@���@�j@�+@�^5@��@�x�@̴9@���@ʇ+@�{@Ɂ@�j@�Z@���@�v�@�@�Ĝ@��m@���@�$�@��@��T@��7@��`@� �@��;@�K�@�+@���@�{@���@�G�@���@�Q�@�C�@�+@��@�x�@�&�@�&�@�j@�ƨ@�K�@���@�v�@� �@�X@�9X@���@��\@��#@�&�@��m@��@�dZ@�ff@��-@���@�1@��
@���@��@��R@���@�v�@�@��@��
@�C�@��R@�5?@���@��w@�t�@�S�@��@���@�{@�p�@���@�z�@�Z@���@�t�@�"�@���@�~�@�E�@�-@�-@��@��T@�`B@�V@��@�Ĝ@���@�A�@�1@��m@��w@��@��+@��@���@��h@�G�@��`@��j@���@��u@��D@��@��w@�dZ@�;d@�
=@��@���@���@��@��#@�x�@�?}@��/@� �@���@���@��@��;@���@�t�@�\)@�33@��H@���@���@�ȴ@���@��+@�~�@�n�@�M�@��@�@�X@��@��@��@�Q�@�1'@��@�  @���@��@��@�|�@�C�@���@�5?@�5?@��7@��@���@�Ĝ@��9@��@�Q�@��w@��@��@�+@�o@���@���@�V@�@�@��@���@���@�`B@�7L@�/@�&�@�%@��9@��@�r�@�Q�@�A�@�(�@�;@�P@~�y@~{@}O�@|��@|�D@|I�@{ƨ@{t�@{S�@{33@{"�@{@z�@z��@zJ@y�7@yX@xĜ@xr�@xbN@xQ�@x1'@x  @w\)@v��@v��@vV@v$�@u�@u@up�@uO�@u�@uV@t��@t��@t�@s@r��@r��@r�!@r��@r�\@rn�@rM�@rJ@q��@q7L@p�9@pr�@pbN@pbN@pA�@pb@o�@o�w@o;d@n�R@m�@m�-@m�@lz�@k��@kC�@jM�@j�@i�7@h�`@hr�@h �@f��@fV@f{@e�@e@e�@e?}@d�/@d9X@d�@cƨ@c��@c��@c��@c��@cC�@b��@a��@ahs@`��@`r�@_�w@_��@_�P@_l�@_K�@_+@^�y@^E�@^{@]@]�h@]`B@\�@\z�@[t�@Z��@Z��@Z^5@Y��@Y��@YX@Y�@X��@Xr�@XA�@W�@W�@W\)@V��@V��@V5?@UO�@T��@T�j@T�D@T�D@TZ@S�
@SC�@So@So@So@S@R�H@R��@Rn�@Q�@Q�#@Q��@Qhs@Q7L@Q&�@Q�@P��@PĜ@P��@P�u@P�u@P�@PA�@Pb@P  @O�w@O;d@Nff@N{@M�@M�@M?}@L��@L�@L�/@L�j@L��@LZ@LI�@L1@K�@KC�@J�@J�\@JM�@I��@I�^@I��@I��@I7L@H��@H�u@HbN@H1'@Hb@H  @G�w@G�P@GK�@F�R@F@E�h@E�@Dz�@C�m@Co@B��@A�@A7L@A%@@�`@@�9@@bN@?��@?�w@?�w@?�P@?l�@?\)@>��@>ff@>E�@>@=��@<��@<j@<(�@;t�@;"�@;o@;@:��@:=q@9��@9x�@9&�@8��@8�@8bN@8 �@7��@7K�@7�@7
=@6��@6$�@5�T@5�T@5��@5p�@5V@4�@4�/@4�@4�D@49X@4�@3�F@3�@3S�@333@3o@2�H@2�!@2~�@2M�@2J@1�@1��@1��@1hs@1G�@1&�@0�`@0��@0�9@0��@0�@0Q�@01'@/�;@/�P@/+@/
=@.ȴ@.��@.��@.�+@.V@-@-?}@,��@,�j@,Z@,9X@,9X@+�
@+C�@+"�@+@*�@*��@*�!@)��@)x�@)%@(�u@(bN@'�@'l�@'K�@'K�@'K�@'\)@'K�@&�R@&V@&E�@&@%@%��@%`B@$��@$�/@$��@$��@$��@$z�@$I�@$�@$�@$�@$�@$�@#�m@#t�@#o@"��@"�\@!�@!x�@!7L@!%@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ bN@�w@|�@\)@K�@;d@+@��@�R@v�@V@5?@{@`B@?}@?}@?}@V@�@Z@(�@�@�@�@1@�m@�m@ƨ@ƨ@ƨ@�F@�F@�F@t�@�@�H@��@M�@�@�7@hs@X@�@�9@�@r�@r�@bN@A�@  @�w@;d@�+@ff@ff@V@$�@�-@�@�/@�j@�@�@��@��@�D@z�@(�@ƨ@��@dZ@o@�H@��@��@~�@=q@J@�^@�7@X@&�@%@%@��@��@�9@�@r�@1'@�;@�P@;d@
=@ȴ@�+@v�@ff@ff@ff@V@E�@E�@E�@$�@@@`B@/@�j@��@Z@�@��@�
@�F@�@t�@"�@
�!@
�\@
~�@
M�@
=q@
�@	��@	��@	��@	��@	hs@	G�@	7L@	�@�`@��@Ĝ@�u@1'@b@  @  @�@�;@��@��@�@�P@l�@;d@+@+@�@�@��@ȴ@��@�+@v�@v�@ff@V@$�@@�@��@�@`B@?}@�@��@��@�@�D@z�@1@��@S�@C�@33@"�@@��@��@�\@^5@M�@=q@-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBB��B��B�qB�XB�-B��B��B��B��B��B��B��B�oB�bB�\B�PB�bB�JB�JB�=B�PB�VB�JB�DB�=B�VB�VB�\B�VB�PB�PB�VB�VB�PB�PB�JB�DB�B|�By�Bx�Bx�Bu�Br�Bo�Bk�Bl�Bk�Bk�Bk�BjBhsBgmBgmBffBe`BaHBT�BE�B>wB6FB1'B(�B�B�BuBB��B�yB�;B�B��B��BȴBĜBÖB��B�wB�9B�B��B�bB{�Bz�Bv�Bk�BbNBYBJ�B.B$�B�B
=B
�B
�#B
ɺB
�}B
�RB
�?B
�-B
�B
��B
��B
�\B
�1B
{�B
gmB
XB
N�B
B�B
:^B
6FB
/B
)�B
#�B
�B
	7B
B	��B	�B	�/B	�
B	��B	��B	��B	ȴB	��B	�dB	�'B	��B	�\B	x�B	t�B	o�B	n�B	jB	dZB	\)B	VB	S�B	Q�B	O�B	M�B	F�B	B�B	;dB	0!B	&�B	�B	�B	oB	bB	\B	DB	B	  B��B��B�B�sB�ZB�5B�#B�B��B��B��BɺBƨBŢBB��B�}B�jB�^B�RB�LB�9B�3B�-B�'B�B�B�B��B��B��B��B��B��B�{B�oB�VB�PB�DB�7B�1B�%B�B�B� B}�By�Bm�BjBe`BdZBcTBaHB_;B]/B\)BZBZBYBT�BT�BS�BR�BR�BO�BN�BM�BJ�BI�BG�BE�BE�BE�BE�BE�BC�BC�BB�B@�B>wB<jB;dB9XB:^B<jB?}BE�BG�BI�BF�BA�BF�BH�BJ�BK�BL�BM�BN�BP�BP�BP�BP�BR�BQ�BQ�BR�BS�BT�BW
BW
BW
BZBZB[#B[#B[#BZB\)B`BBaHB`BB`BBaHBbNBbNBbNBbNBaHB`BB`BBaHBdZBffBk�Bm�Bl�Bl�Bl�Bn�Bu�Bu�Bu�Bv�Bx�Bz�By�B{�B{�B|�B~�B� B�B�B� B~�B� B�B�B�B�B�B�B�B� B}�B� B� B|�B|�B~�B�B�B�B� B{�B{�B{�B{�B~�B~�B~�B� B� B� B~�B~�B�B�B�B�1B�=B�\B�uB�{B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�3B�9B�FB�^B�qB�wB�}B��BÖBĜBŢBƨB��B��B��B�
B�
B�B�/B�5B�;B�;B�BB�ZB�fB�B�B�B�B�B�B��B��B��B��B	B	
=B	JB	PB	PB	VB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	'�B	+B	1'B	49B	6FB	9XB	;dB	;dB	<jB	<jB	=qB	=qB	=qB	=qB	>wB	B�B	D�B	H�B	M�B	P�B	VB	W
B	XB	XB	\)B	bNB	dZB	gmB	k�B	k�B	o�B	q�B	t�B	x�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�DB	�JB	�PB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�9B	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	�dB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	B	ĜB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
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
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
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
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
.B
.B
/B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
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
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
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
F�B
F�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
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
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
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
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
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
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
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
p�B
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
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBB��B��B�qB�XB�-B��B��B��B��B��B��B��B�oB�bB�\B�PB�bB�JB�JB�=B�PB�VB�JB�DB�=B�VB�VB�\B�VB�PB�PB�VB�VB�PB�PB�JB�DB�B|�By�Bx�Bx�Bu�Br�Bo�Bk�Bl�Bk�Bk�Bk�BjBhsBgmBgmBffBe`BaHBT�BE�B>wB6FB1'B(�B�B�BuBB��B�yB�;B�B��B��BȴBĜBÖB��B�wB�9B�B��B�bB{�Bz�Bv�Bk�BbNBYBJ�B.B$�B�B
=B
�B
�#B
ɺB
�}B
�RB
�?B
�-B
�B
��B
��B
�\B
�1B
{�B
gmB
XB
N�B
B�B
:^B
6FB
/B
)�B
#�B
�B
	7B
B	��B	�B	�/B	�
B	��B	��B	��B	ȴB	��B	�dB	�'B	��B	�\B	x�B	t�B	o�B	n�B	jB	dZB	\)B	VB	S�B	Q�B	O�B	M�B	F�B	B�B	;dB	0!B	&�B	�B	�B	oB	bB	\B	DB	B	  B��B��B�B�sB�ZB�5B�#B�B��B��B��BɺBƨBŢBB��B�}B�jB�^B�RB�LB�9B�3B�-B�'B�B�B�B��B��B��B��B��B��B�{B�oB�VB�PB�DB�7B�1B�%B�B�B� B}�By�Bm�BjBe`BdZBcTBaHB_;B]/B\)BZBZBYBT�BT�BS�BR�BR�BO�BN�BM�BJ�BI�BG�BE�BE�BE�BE�BE�BC�BC�BB�B@�B>wB<jB;dB9XB:^B<jB?}BE�BG�BI�BF�BA�BF�BH�BJ�BK�BL�BM�BN�BP�BP�BP�BP�BR�BQ�BQ�BR�BS�BT�BW
BW
BW
BZBZB[#B[#B[#BZB\)B`BBaHB`BB`BBaHBbNBbNBbNBbNBaHB`BB`BBaHBdZBffBk�Bm�Bl�Bl�Bl�Bn�Bu�Bu�Bu�Bv�Bx�Bz�By�B{�B{�B|�B~�B� B�B�B� B~�B� B�B�B�B�B�B�B�B� B}�B� B� B|�B|�B~�B�B�B�B� B{�B{�B{�B{�B~�B~�B~�B� B� B� B~�B~�B�B�B�B�1B�=B�\B�uB�{B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�3B�9B�FB�^B�qB�wB�}B��BÖBĜBŢBƨB��B��B��B�
B�
B�B�/B�5B�;B�;B�BB�ZB�fB�B�B�B�B�B�B��B��B��B��B	B	
=B	JB	PB	PB	VB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	#�B	'�B	+B	1'B	49B	6FB	9XB	;dB	;dB	<jB	<jB	=qB	=qB	=qB	=qB	>wB	B�B	D�B	H�B	M�B	P�B	VB	W
B	XB	XB	\)B	bNB	dZB	gmB	k�B	k�B	o�B	q�B	t�B	x�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�DB	�JB	�PB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�9B	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	�dB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	B	ĜB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
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
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
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
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
.B
.B
/B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
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
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
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
F�B
F�B
F�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
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
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
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
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
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
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
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
p�B
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
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220529100120                              AO  ARCAADJP                                                                    20220529100120    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220529100120  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220529100120  QCF$                G�O�G�O�G�O�0               