CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:04Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140804  20181024140804  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ע�{1   @ע�Jf@4.��O�;�c����o1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@���A   AffA@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D	  D	y�D
  D
� D  Dy�D  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  Dy�D  D�fDfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$y�D%  D%�fD&  D&y�D&��D'y�D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=y�D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DXy�DY  DY� DZ  DZ� D[fD[�fD\  D\� D]  D]� D]��D^� D_  D_�fD`  D`� Da  Da� Da��Dby�Db��Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dg��Dhy�Di  Di�fDjfDj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� DofDo� Do��Dpy�Dq  Dq� DrfDr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dyj=D�;3D�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�\)AG�A�AAG�AaG�A���A��
A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B���B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C��C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<.C>{C@.CB.CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV.CX{CZ{C\{C^{C`.Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cy��C{��C~{C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
C�
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
C�
C�
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
=C�
=C�
=C��pC��pC�
=C�
=C�
C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
C�
C�
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
=C�
C�
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
=C�
C�
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
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C��pC��pD D �DD�DD�DD�DD�D��D�DD�DD��DD�D	D	~�D
D
�DD~�DD�DD�D��D�D�D�DD�DD�DD�DD�D��D�DD�D��D�DD�DD�DD~�DD��D�D�DD�DD�DD�DD�D D �D!D!�D!��D"�D#D#�D$D$~�D%D%��D&D&~�D&��D'~�D(D(�D)D)�D*D*�D+D+�D+��D,�D-D-~�D.D.�D/D/�D0D0�D1D1�D2D2~�D3D3�D4D4�D5D5�D5��D6�D7D7�D8D8�D9D9�D:D:�D;D;~�D<D<�D=D=~�D>D>�D?D?��D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DP�DP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DW��DX~�DYDY�DZDZ�D[�D[��D\D\�D]D]�D]��D^�D_D_��D`D`�DaDa�Da��Db~�Db��Dc�DdDd��DeDe�DfDf�DgDg�Dg��Dh~�DiDi��Dj�Dj�DkDk�DlDl�DmDm��DnDn�Do�Do�Do��Dp~�DqDq�Dr�Dr��DsDs�DtDt�DuDu�DvDv�DwDw�Dyo\D�=�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�r�A�p�A�x�ÃA̅Ả7A�5?A��mAʗ�Aʣ�AʮAʩ�AʑhA�ffA�VA�G�A�+A��A�oA�bA�bA�bA�%A��TAə�A�\)A���A��A��#AǺ^A�z�A�VA��A���AƶFA���A���A�ƨA�x�A�A�Aţ�A�n�A�Q�A�VA�XA�  Aģ�A�=qA��A�ƨA�33A���A§�A��7A���A�G�A�p�A�VA�z�A��\A�VA���A���A�A�A�
=A��A��yA��mA�^5A��/A�(�A�|�A��A�A�l�A� �A���A��A��-A��A��+A��TA���A���A�S�A��A�5?A�;dA�O�A�Q�A��FA��A�ȴA�33A�%A�&�A���A�`BA�ȴA�ȴA���A���A�33A�n�A�l�A�VA��A�1'A�ȴA�VA|�A}�wA{t�Az��Ay��AxbNAv�\At�jAs��Aq�Am��Aj�uAg�
Af��Ac|�A_t�A[7LAYVAVjAU��AS�AQ�AM�AIt�ACO�A?+A<ZA9�
A6�RA5�A4{A2�uA2bA1�PA/�PA+��A+A*�!A*^5A*{A)�A)K�A)�A(��A(�A&�yA$�A#t�A"ĜA!t�A ��A E�A��AK�A�yA��A�`A��A��AJA�^A�PA(�A��AA-A��AVA{AƨA;dA��A��AVAn�A?}A�jA�#A�7A��A�7A`BA
Q�A��A�AĜA"�A�TA�wA��A|�A+A�wA ^5@��w@�@���@���@�?}@�z�@�@���@�Ĝ@�Z@�K�@�E�@�@�9@��@��@��@�Ĝ@�I�@�w@�o@�!@�$�@�7@��@��`@���@䛦@�9X@�;d@���@�+@��T@���@��@ݲ-@�\)@��T@���@؋D@�  @��@ԛ�@ӍP@�+@��@�v�@���@�b@���@�=q@�@��#@͡�@͑h@�O�@��/@� �@��y@�^5@��#@��#@��T@�Ĝ@��@�{@Ĵ9@��@��;@���@�o@��h@��u@��w@���@�^5@���@��@���@��9@�I�@�;d@���@��+@�^5@�$�@�{@���@���@��H@�n�@�E�@��@��T@��^@�V@�Z@���@��P@�t�@��@�bN@�V@�/@�X@���@���@�%@�Z@�b@�"�@��7@��`@�z�@�b@���@�ƨ@���@��
@��m@�  @�1@���@�dZ@�;d@��R@�M�@�{@���@�?}@��/@�r�@��@��m@��;@���@��@�=q@���@�hs@�V@��`@��9@�bN@�A�@��w@���@�J@��@�@�$�@�5?@���@���@��7@�G�@��`@�Z@��@�K�@�K�@�;d@�@�{@�@��^@��-@���@��h@�p�@�/@���@��u@�9X@� �@��@�ƨ@��F@���@��@�S�@�K�@�"�@���@��H@��@�ȴ@��R@���@�~�@�=q@��@��@���@�X@�%@���@��@��@��F@�C�@�M�@��#@��^@���@�p�@��@���@��9@���@��`@�Ĝ@���@�9X@��@�S�@���@�@��7@�`B@�G�@�?}@�/@��/@��9@���@�t�@�;d@�
=@��R@�n�@�M�@�$�@�@��@�@��7@��7@��7@�x�@�`B@��/@��9@�bN@� �@�ƨ@�K�@��H@�ȴ@��!@���@��\@�~�@�ff@�M�@�=q@���@��T@��-@�`B@�O�@�O�@���@���@��D@�r�@��F@�M�@�@�X@��@���@��j@��@���@���@���@��D@�Q�@��@�\)@��R@�E�@�5?@�=q@�-@�$�@�@�A�@t�K@]�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�p�A�r�A�p�A�x�ÃA̅Ả7A�5?A��mAʗ�Aʣ�AʮAʩ�AʑhA�ffA�VA�G�A�+A��A�oA�bA�bA�bA�%A��TAə�A�\)A���A��A��#AǺ^A�z�A�VA��A���AƶFA���A���A�ƨA�x�A�A�Aţ�A�n�A�Q�A�VA�XA�  Aģ�A�=qA��A�ƨA�33A���A§�A��7A���A�G�A�p�A�VA�z�A��\A�VA���A���A�A�A�
=A��A��yA��mA�^5A��/A�(�A�|�A��A�A�l�A� �A���A��A��-A��A��+A��TA���A���A�S�A��A�5?A�;dA�O�A�Q�A��FA��A�ȴA�33A�%A�&�A���A�`BA�ȴA�ȴA���A���A�33A�n�A�l�A�VA��A�1'A�ȴA�VA|�A}�wA{t�Az��Ay��AxbNAv�\At�jAs��Aq�Am��Aj�uAg�
Af��Ac|�A_t�A[7LAYVAVjAU��AS�AQ�AM�AIt�ACO�A?+A<ZA9�
A6�RA5�A4{A2�uA2bA1�PA/�PA+��A+A*�!A*^5A*{A)�A)K�A)�A(��A(�A&�yA$�A#t�A"ĜA!t�A ��A E�A��AK�A�yA��A�`A��A��AJA�^A�PA(�A��AA-A��AVA{AƨA;dA��A��AVAn�A?}A�jA�#A�7A��A�7A`BA
Q�A��A�AĜA"�A�TA�wA��A|�A+A�wA ^5@��w@�@���@���@�?}@�z�@�@���@�Ĝ@�Z@�K�@�E�@�@�9@��@��@��@�Ĝ@�I�@�w@�o@�!@�$�@�7@��@��`@���@䛦@�9X@�;d@���@�+@��T@���@��@ݲ-@�\)@��T@���@؋D@�  @��@ԛ�@ӍP@�+@��@�v�@���@�b@���@�=q@�@��#@͡�@͑h@�O�@��/@� �@��y@�^5@��#@��#@��T@�Ĝ@��@�{@Ĵ9@��@��;@���@�o@��h@��u@��w@���@�^5@���@��@���@��9@�I�@�;d@���@��+@�^5@�$�@�{@���@���@��H@�n�@�E�@��@��T@��^@�V@�Z@���@��P@�t�@��@�bN@�V@�/@�X@���@���@�%@�Z@�b@�"�@��7@��`@�z�@�b@���@�ƨ@���@��
@��m@�  @�1@���@�dZ@�;d@��R@�M�@�{@���@�?}@��/@�r�@��@��m@��;@���@��@�=q@���@�hs@�V@��`@��9@�bN@�A�@��w@���@�J@��@�@�$�@�5?@���@���@��7@�G�@��`@�Z@��@�K�@�K�@�;d@�@�{@�@��^@��-@���@��h@�p�@�/@���@��u@�9X@� �@��@�ƨ@��F@���@��@�S�@�K�@�"�@���@��H@��@�ȴ@��R@���@�~�@�=q@��@��@���@�X@�%@���@��@��@��F@�C�@�M�@��#@��^@���@�p�@��@���@��9@���@��`@�Ĝ@���@�9X@��@�S�@���@�@��7@�`B@�G�@�?}@�/@��/@��9@���@�t�@�;d@�
=@��R@�n�@�M�@�$�@�@��@�@��7@��7@��7@�x�@�`B@��/@��9@�bN@� �@�ƨ@�K�@��H@�ȴ@��!@���@��\@�~�@�ff@�M�@�=q@���@��T@��-@�`B@�O�@�O�@���@���@��D@�r�@��F@�M�@�@�X@��@���@��j@��@���@���@���@��D@�Q�@��@�\)@��R@�E�@�5?@�=q@�-@�$�@�@�A�@t�K@]�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
%B
%B
%B
%B
%B
+B
+B
$�B
��B
�FB
B
��B
�TB
�mB
�sB
�sB
�mB
�yB
�B
�B
�B
�B
��B
��B
��B1B�B-B8RB;dBB�BL�BQ�BP�BQ�BcTBm�B� B�B�%B{�B� B�+B�JB�uB��B��B�!B�}B��B�#B�BPB�B"�B9XB'�B�B�B&�B&�B49BC�BC�BD�BH�BI�BM�BM�B]/BdZBcTBcTBbNB]/BXBYB\)BS�BF�B@�B9XB33B)�B%�B�B{BB�B�NB��BÖB�wB�!B��Bx�B[#BI�B?}B9XB/B�BJB
��B
��B
�'B
��B
�%B
r�B
bNB
M�B
5?B
$�B
�B
	7B
B	��B	�B	�sB	�)B	��B	ŢB	�B	��B	�VB	�B	q�B	\)B	H�B	=qB	2-B	,B	'�B	 �B	\B��B�BBǮB�}B�^B�3B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�9B�?B�?B�?B�wB�}B�qB�qBBĜBĜBÖBȴBƨBÖB�wB�RB�?B�3B�-B�'B�!B�B�FB�wB�wB�qB�-B�B�B��B��B��B��B��B�B�?B�RB�^B�dB�jB�jB�dB�jB�jB�jB�jB�dB�dB�dB�dB�^B�^B�dB�qB�qB�wB�}B��B��BÖBŢBƨBƨBƨBȴB��B��B��B��B��B��B��B�B�B�B�B�#B�#B�#B�)B�5B�NB�TB�ZB�`B�`B�yB�B�B�B�B�B�B��B��B	B	B		7B	PB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	/B	1'B	49B	5?B	5?B	9XB	>wB	E�B	K�B	P�B	XB	^5B	bNB	cTB	e`B	hsB	iyB	hsB	gmB	gmB	e`B	cTB	e`B	gmB	iyB	k�B	l�B	l�B	n�B	o�B	r�B	t�B	w�B	x�B	z�B	~�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�JB	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�9B	�FB	�LB	�XB	�^B	��B	ÖB	ŢB	ƨB	ǮB	ƨB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�BB	�HB	�TB	�TB	�ZB	�fB	�mB	�fB	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B

=B
DB
DB
DB
DB
DB

=B
DB
JB
PB
\B
bB
bB
bB
bB
bB
hB
�B
%B
7�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
%B
%B
%B
%B
%B
+B
+B
$�B
��B
�FB
B
��B
�TB
�mB
�sB
�sB
�mB
�yB
�B
�B
�B
�B
��B
��B
��B1B�B-B8RB;dBB�BL�BQ�BP�BQ�BcTBm�B� B�B�%B{�B� B�+B�JB�uB��B��B�!B�}B��B�#B�BPB�B"�B9XB'�B�B�B&�B&�B49BC�BC�BD�BH�BI�BM�BM�B]/BdZBcTBcTBbNB]/BXBYB\)BS�BF�B@�B9XB33B)�B%�B�B{BB�B�NB��BÖB�wB�!B��Bx�B[#BI�B?}B9XB/B�BJB
��B
��B
�'B
��B
�%B
r�B
bNB
M�B
5?B
$�B
�B
	7B
B	��B	�B	�sB	�)B	��B	ŢB	�B	��B	�VB	�B	q�B	\)B	H�B	=qB	2-B	,B	'�B	 �B	\B��B�BBǮB�}B�^B�3B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�9B�?B�?B�?B�wB�}B�qB�qBBĜBĜBÖBȴBƨBÖB�wB�RB�?B�3B�-B�'B�!B�B�FB�wB�wB�qB�-B�B�B��B��B��B��B��B�B�?B�RB�^B�dB�jB�jB�dB�jB�jB�jB�jB�dB�dB�dB�dB�^B�^B�dB�qB�qB�wB�}B��B��BÖBŢBƨBƨBƨBȴB��B��B��B��B��B��B��B�B�B�B�B�#B�#B�#B�)B�5B�NB�TB�ZB�`B�`B�yB�B�B�B�B�B�B��B��B	B	B		7B	PB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	/B	1'B	49B	5?B	5?B	9XB	>wB	E�B	K�B	P�B	XB	^5B	bNB	cTB	e`B	hsB	iyB	hsB	gmB	gmB	e`B	cTB	e`B	gmB	iyB	k�B	l�B	l�B	n�B	o�B	r�B	t�B	w�B	x�B	z�B	~�B	� B	�B	�B	�B	�%B	�+B	�7B	�DB	�JB	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�9B	�FB	�LB	�XB	�^B	��B	ÖB	ŢB	ƨB	ǮB	ƨB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�BB	�HB	�TB	�TB	�ZB	�fB	�mB	�fB	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B

=B
DB
DB
DB
DB
DB

=B
DB
JB
PB
\B
bB
bB
bB
bB
bB
hB
�B
%B
7�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140804                              AO  ARCAADJP                                                                    20181024140804    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140804  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140804  QCF$                G�O�G�O�G�O�0               