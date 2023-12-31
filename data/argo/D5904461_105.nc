CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-09T03:15:59Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160309031559  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               iA   AO  5286_8897_105                   2C  D   APEX                            6531                            072314                          846 @כw)��1   @כw���f@2�=p��
�c`�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    iA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffC  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�	�D�C3D�� D��3D�  D�@ D��fD�ɚD��D�C3D���D��fD�fD�\�Dڌ�D��D�3D�6fD�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B��\C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&.C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
=C�
C�
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
=C��pC�
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
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>�D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt~�Dy��D�)D�E�D���D���D��D�B�D���D��)D�\D�E�D��\D���D��D�_\Dڏ\D�\D��D�8�D�o\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aț�Aȏ\A�z�A�v�A�r�A�p�A�hsA�ffA�^5A�XA�O�A��A�ȴA�|�A�9XA�
=A���A��A��yA�bA�Q�A�|�Aǥ�Aǟ�A�hsA�I�A�bA��AƸRAƣ�A�|�A�VA�I�A�G�A�I�A�G�A�G�A�G�A�C�A�9XA�1'A��A�A��A���A��TAŬAŰ!A�XA��`A���A�ĜA�x�A�AÛ�A�ffA�K�A�{A�ffA�9XA���A�O�A�E�A�E�A��`A�A��PA�x�A���A�hsA�|�A�1A��FA�hsA�l�A�A�ƨA�r�A��
A�M�A��;A���A���A���A�-A���A�XA�7LA�A��DA�oA�ZA��A�bA��+A��PA�E�A�%A��A�v�A�~�A���A�A�A}��A{\)Ax^5Au�At�Ap��AlA�Aj�Ae
=Ac�hAbȴA_/AZ��AYC�AW%AT��AQ&�AN�\AL �AJ1AH��AG`BAD�`A@��A>v�A>jA>r�A>bNA>ZA>VA>M�A>M�A>Q�A>M�A>VA>Q�A>VA>VA>ZA>ZA>E�A=t�A:ffA9A8n�A7A5t�A2I�A/�#A/�A.ȴA.~�A,�A+��A+/A+O�A+�A*�A*��A+
=A+;dA+?}A+?}A*�A(Q�A$�uA"1'A!�hAƨAA�AS�A^5A��AjAVA�A�A��A�!AE�A�\A�mA�A�+A�A�AC�A�AȴA�jA~�A�FAȴAK�A��Av�A�Al�A
ȴA	A=qA^5A��A�A��A�AoAQ�A9XA��A7LA V@��`@�@�ȴ@��D@��@�`B@�u@�(�@��@�Ĝ@�p�@���@�\)@ݡ�@ܣ�@�r�@܃@܋D@�\)@ڏ\@�hs@��@��@�1'@Ӿw@�
=@�5?@�Z@�C�@�x�@���@�|�@�ȴ@�-@���@�"�@��H@Ə\@���@�O�@��`@� �@¸R@�-@��^@�&�@���@��@�
=@�ff@���@��@���@�A�@�b@���@��@�A�@�(�@���@��@��@�J@�hs@���@��P@���@�{@�/@��!@��@�O�@�7L@��@��/@��@�I�@�  @�S�@��H@��+@��-@�%@�Ĝ@�r�@��
@�K�@��H@���@�^5@�{@��#@�`B@��@��D@�Z@��@�;d@��@�
=@���@��@��H@���@�E�@�J@��@��@��T@��#@��-@�x�@�hs@���@��@���@��@��H@�v�@��@�@�x�@�`B@�G�@��`@��@�  @���@��y@�v�@�{@��^@���@�hs@�O�@�G�@�/@�%@�Ĝ@�bN@��m@��w@�|�@�l�@�S�@�o@���@�M�@���@�X@��@��9@�z�@�I�@��
@�;d@���@��T@��7@�O�@��@���@��@�Z@���@�|�@�t�@�dZ@�K�@�;d@�33@��@���@�^5@�V@�E�@�-@��#@���@��@�x�@�`B@�X@�G�@�V@���@��9@���@�z�@�Q�@�1'@�  @��
@��P@�t�@�S�@�K�@�S�@�;d@�"�@��@��\@�ff@��@��@��#@��7@��@���@�r�@��m@�ƨ@�ƨ@��w@��@�
=@�ȴ@�ff@�$�@�@���@�`B@�G�@�/@���@��u@�bN@� �@���@���@��@�|�@�"�@���@��\@�V@��@��#@���@�/@��@��;@�ƨ@�t�@�;d@��@�@��+@�^5@�E�@��@���@�`B@�&�@��@�%@��`@���@���@�Q�@�b@�;@�w@��@\)@~�@~�+@~ff@}�@}O�@|��@|I�@yX@n5?@eV@` �@X�9@QX@K�@BJ@;@4�/@0b@)�^@&�y@ ��@�@�@�/@
=@"�@�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aț�Aȏ\A�z�A�v�A�r�A�p�A�hsA�ffA�^5A�XA�O�A��A�ȴA�|�A�9XA�
=A���A��A��yA�bA�Q�A�|�Aǥ�Aǟ�A�hsA�I�A�bA��AƸRAƣ�A�|�A�VA�I�A�G�A�I�A�G�A�G�A�G�A�C�A�9XA�1'A��A�A��A���A��TAŬAŰ!A�XA��`A���A�ĜA�x�A�AÛ�A�ffA�K�A�{A�ffA�9XA���A�O�A�E�A�E�A��`A�A��PA�x�A���A�hsA�|�A�1A��FA�hsA�l�A�A�ƨA�r�A��
A�M�A��;A���A���A���A�-A���A�XA�7LA�A��DA�oA�ZA��A�bA��+A��PA�E�A�%A��A�v�A�~�A���A�A�A}��A{\)Ax^5Au�At�Ap��AlA�Aj�Ae
=Ac�hAbȴA_/AZ��AYC�AW%AT��AQ&�AN�\AL �AJ1AH��AG`BAD�`A@��A>v�A>jA>r�A>bNA>ZA>VA>M�A>M�A>Q�A>M�A>VA>Q�A>VA>VA>ZA>ZA>E�A=t�A:ffA9A8n�A7A5t�A2I�A/�#A/�A.ȴA.~�A,�A+��A+/A+O�A+�A*�A*��A+
=A+;dA+?}A+?}A*�A(Q�A$�uA"1'A!�hAƨAA�AS�A^5A��AjAVA�A�A��A�!AE�A�\A�mA�A�+A�A�AC�A�AȴA�jA~�A�FAȴAK�A��Av�A�Al�A
ȴA	A=qA^5A��A�A��A�AoAQ�A9XA��A7LA V@��`@�@�ȴ@��D@��@�`B@�u@�(�@��@�Ĝ@�p�@���@�\)@ݡ�@ܣ�@�r�@܃@܋D@�\)@ڏ\@�hs@��@��@�1'@Ӿw@�
=@�5?@�Z@�C�@�x�@���@�|�@�ȴ@�-@���@�"�@��H@Ə\@���@�O�@��`@� �@¸R@�-@��^@�&�@���@��@�
=@�ff@���@��@���@�A�@�b@���@��@�A�@�(�@���@��@��@�J@�hs@���@��P@���@�{@�/@��!@��@�O�@�7L@��@��/@��@�I�@�  @�S�@��H@��+@��-@�%@�Ĝ@�r�@��
@�K�@��H@���@�^5@�{@��#@�`B@��@��D@�Z@��@�;d@��@�
=@���@��@��H@���@�E�@�J@��@��@��T@��#@��-@�x�@�hs@���@��@���@��@��H@�v�@��@�@�x�@�`B@�G�@��`@��@�  @���@��y@�v�@�{@��^@���@�hs@�O�@�G�@�/@�%@�Ĝ@�bN@��m@��w@�|�@�l�@�S�@�o@���@�M�@���@�X@��@��9@�z�@�I�@��
@�;d@���@��T@��7@�O�@��@���@��@�Z@���@�|�@�t�@�dZ@�K�@�;d@�33@��@���@�^5@�V@�E�@�-@��#@���@��@�x�@�`B@�X@�G�@�V@���@��9@���@�z�@�Q�@�1'@�  @��
@��P@�t�@�S�@�K�@�S�@�;d@�"�@��@��\@�ff@��@��@��#@��7@��@���@�r�@��m@�ƨ@�ƨ@��w@��@�
=@�ȴ@�ff@�$�@�@���@�`B@�G�@�/@���@��u@�bN@� �@���@���@��@�|�@�"�@���@��\@�V@��@��#@���@�/@��@��;@�ƨ@�t�@�;d@��@�@��+@�^5@�E�@��@���@�`B@�&�@��@�%@��`@���@���@�Q�@�b@�;@�w@��@\)@~�@~�+@~ff@}�@}O�@|��G�O�@yX@n5?@eV@` �@X�9@QX@K�@BJ@;@4�/@0b@)�^@&�y@ ��@�@�@�/@
=@"�@�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�!B	�!B	�-B	�FB	�dB	ĜB	��B	��B	�B	�NB	��B
\B
'�B
>wB
?}B
E�B
H�B
P�B
XB
\)B
`BB
ffB
iyB
k�B
m�B
m�B
n�B
n�B
o�B
o�B
s�B
z�B
�%B
�\B
��B
�LB
�wB
ŢB
��B
�mB
��BVB�B8RBs�B�B�JB�PB�7B�1B��B�3B�B��B�B�B��B��B�B��B��B��B�\B�\B��B�B�B��B��B�\B�Bw�Bn�BbNBF�BD�B+B
��B
�ZB
��B
ŢB
�!B
�bB
�B
{�B
r�B
cTB
O�B
I�B
%�B
\B	�mB	�B	ǮB	�9B	��B	�hB	}�B	p�B	\)B	E�B	8RB	!�B	�B	uB	+B��B��B��B��B�B�sB�fB�ZB�NB�BB�#B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B�
B��B��BĜB�jB�wB�wB��B��B�wBƨBȴB��B��B��B�B�
B�#B�/B�TB�BB�#B�5B�#B�B�B��B��B��BĜB�wB�B��B�!B�LB�dB�dB�XB�}BŢBÖBÖBÖBÖBĜBƨBǮBǮBǮBƨBŢB��B�}B�jB�XB�^B�XB�!B�B��B��B��B��B��B��B��B��B��B��B�hB�%B�B}�Bx�Bo�BiyBk�BiyBhsBiyBiyBr�B|�B}�B~�B� B�B�7B�=B�7B�7B�hB�hB�oB�hB�hB�hB�VB�JB�\B�oB��B��B��B��B��B��B��B��B��B��B�B�!B�-B�3B�?B�FB�XB�qB�}BBŢBɺB��B��B�B�TB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	
=B	VB	\B	hB	�B	�B	�B	�B	!�B	$�B	&�B	(�B	+B	/B	0!B	6FB	9XB	:^B	:^B	;dB	;dB	;dB	<jB	>wB	?}B	@�B	@�B	A�B	A�B	B�B	D�B	D�B	K�B	P�B	T�B	XB	YB	^5B	aHB	e`B	ffB	ffB	ffB	gmB	iyB	l�B	n�B	q�B	r�B	u�B	y�B	{�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�+B	�1B	�1B	�=B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�?B	�FB	�RB	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�qB	��B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�5B	�;B	�BB	�BB	�BB	�HB	�TB	�ZB	�`B	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
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
+B
+B
+B
1B
	7B
	7B

=B

=B
DB
JB
VB
�B
"�B
&�B
.B
33B
9XB
A�B
H�B
N�B
S�B
YB
\)B
bNB
dZB
hsB
k�B
r�B
u�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�,B	�,B	�-B	�,B	�*B	�-B	�,B	�+B	�,B	�)B	�)B	�2B	�IB	�jB	ģB	��B	��B	�B	�RB	��B
\B
'�B
>uB
?|B
E�B
H�B
P�B
XB
\)B
`?B
ffB
ivB
k�B
m�B
m�B
n�B
n�B
o�B
o�B
s�B
z�B
�$B
�\B
��B
�KB
�qB
ŞB
��B
�jB
��BUB{B8KBs�B�B�DB�HB�/B�*B��B�/B�B��B��B��B��B��B��B��B��B��B�VB�UB��B�B��B��B��B�UB�Bw�Bn�BbHBF�BD�B*�B
��B
�WB
��B
śB
�B
�_B
�B
{�B
r�B
cPB
O�B
I�B
%�B
\B	�mB	�B	ǯB	�<B	��B	�kB	}�B	p�B	\.B	E�B	8ZB	!�B	�B	}B	3B��B��B��B��B�B�~B�nB�gB�WB�JB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�2B�B��B��BħB�vB��B�B��B��B��BƱBȾB��B� B��B�B�B�+B�6B�^B�JB�+B�>B�,B�(B�B��B��B��BģB��B�$B��B�-B�UB�pB�oB�aB��BūBßBßBÞBÞBĦBưBǶBǶBǵBƯBŭB��B��B�sB�aB�iB�`B�,B�B��B��B��B��B��B��B��B��B��B��B�sB�0B�B}�Bx�Bo�Bi�Bk�Bi�Bh�Bi�Bi�Br�B|�B}�BB�
B�B�AB�GB�CB�AB�qB�sB�zB�tB�qB�rB�_B�SB�gB�xB��B��B��B��B��B��B��B�B��B�B�B�)B�5B�9B�JB�NB�aB�wB��BBŦB��B��B��B�%B�]B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	
B	B	$B	
BB	[B	aB	kB	�B	�B	�B	�B	!�B	$�B	&�B	(�B	+B	/B	0$B	6HB	9XB	:aB	:aB	;gB	;eB	;hB	<kB	>yB	?�B	@�B	@�B	A�B	A�B	B�B	D�B	D�B	K�B	P�B	U B	XB	YB	^6B	aHB	eaB	fgB	ffB	fgB	gmB	izB	l�B	n�B	q�B	r�B	u�B	y�B	{�B	}�B	~�B	~�B	~�B	�B	�B	�B	�B	�*B	�1B	�1B	�<B	�QB	�gB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�)B	�3B	�<B	�BB	�PB	�\B	�]B	�\B	�cB	�cB	�cB	�gB	�mB	��B	��B	��B	��B	��B	B	ęB	šB	ƦB	ƦB	ƤB	ǭB	ȰB	ʿB	ʿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�$B	�4B	�8B	�?B	�>B	�?B	�BB	�QB	�WB	�]B	�iB	�iB	�iB	�mB	�wB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
 B
B
B
B
B
B
B
B
B
B
B
B
B
!B
 B
(B
(B
&B
(B
'B
,B
	3B
	3B

8B

6B
AG�O�B
PB
�B
"�B
&�B
.B
30B
9UB
A�B
H�B
N�B
S�B
YB
\"B
bIB
dTB
hkB
k�B
r�B
u�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160309031559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160309031559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160309031559  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                