CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170854  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؅�4�1   @؅����@7[��S���c����S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BG��BP  BX  B`  Bi33Bo��Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl�Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�(RD�Y�D���D��D�=D�N�D��
D��HD�{D�J�D�\D��)D�!�D�L{Dړ�D��fD�\D�S3D�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@��\@��\AG�A;�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�B�RB'�RB/Q�B7Q�B?Q�BF�BOQ�BWQ�B_Q�Bh�Bn�BwQ�BQ�B���B���B���B���B���B��)B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO��CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�Cg�{Ci�{Ck�Cm�{Co�{Cq�Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�Dn�D�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5{�D5��D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt��Dy��D�"�D�T)D��GD��3D��D�IGD���D���D�
D�ED�y�D�޸D�)D�G
DڎfD���D��D�M�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��HA��#A��#A��
A��;A��TA��HA��`A��yA��yA��A��A��A��A��;AԼjAԕ�A�z�A���Aӕ�A�A�oA��/A��yA�A�A̼jA���Aǧ�A�ƨA�(�A�v�A��uA�M�A�z�A��DA���A��\A��A��-A�{A�K�A�~�A�ffA��A��A��A�l�A���A�p�A�/A�M�A��hA���A�VA�oA���A��
A���A��HA���A��A�?}A�v�A���A�E�A��A��uA���A��yA���A�/A���A�ĜA�^5A�1A��DA��`A�O�A�{A�ffA���A���A�  A��A�1A�
=A���A�;dA�/A�I�A�&�A�jA�M�A�-A��;A�K�A��A~1A|bNA{l�Ay�^Aw��At��Ar�RAq�;Ap��Ao+Am�
Al{AjAh9XAf��Ae�
Ad��AdE�Ac+Aa��A_��A^��A\��AZ�RAZVAY�AY\)AXffAVv�AUXAT�jAT=qAR�/AQAQ
=AP�AP(�AOC�ANJAL�+AKƨAK�PAK\)AJ��AI��AI��AIS�AI33AH��AH�\AH9XAF�`AE�AB��AB�AA��AA?}A@ĜA@M�A?�;A>$�A;�A;��A;oA:n�A8ĜA7�
A6�RA5`BA4ĜA41A2�jA1t�A1�A0��A/�PA.jA-O�A,�A+�A+A*ffA)�#A)&�A(��A(ffA'A'�A&�+A%��A%�7A$�RA#�A#��A#C�A"I�A!&�A �AA33A��AG�A�uAz�AbNA��A?}A��A��A��A~�A=qAp�A$�AO�A��A�A�^AJA �A%AVAl�AffA
��A	x�A	"�AĜAjAK�Az�A-A��A��A�wA%A j@��@��@�1@�ȴ@�%@��@���@�P@�E�@���@���@�?}@�Q�@��@�E�@噚@�p�@�p�@�?}@�(�@�n�@�Z@ߥ�@�J@�K�@���@���@֗�@�X@��@ԓu@ӝ�@�o@҇+@ѡ�@϶F@̼j@��@�t�@���@�=q@�G�@�(�@ǶF@�@���@Ĵ9@�(�@î@¸R@��-@�7L@���@���@�ƨ@���@�+@���@���@��T@���@�ȴ@��7@���@��u@�bN@�b@��P@�l�@�K�@�o@���@��@�/@�%@�  @�t�@�@�{@�%@��@�l�@��@���@�E�@���@���@�7L@���@�1'@��@���@�n�@��@��-@��@�Q�@��@��@�$�@���@���@�7L@��D@���@�t�@��@���@�v�@���@�%@��/@��D@��@��@���@���@�\)@��@�ȴ@��H@��@��!@��\@�^5@���@���@��u@��@�&�@��`@�(�@�1@���@��@�+@�V@�E�@�V@�^5@�E�@�-@���@�x�@�V@�V@�V@�7L@�O�@�`B@�O�@�Q�@�b@��@�b@�(�@�  @�t�@�t�@�dZ@�S�@��H@�V@�$�@�-@���@�x�@�&�@���@�7L@�7L@�7L@��@��@��@��@���@���@��9@�9X@�  @��
@�|�@�33@�
=@��@��@�~�@�n�@�^5@�=q@���@��T@���@���@�p�@�7L@��@�%@�Ĝ@��@�z�@�j@�bN@�Z@�I�@�  @��@�ƨ@���@�|�@�dZ@�+@���@���@�ff@�=q@��#@��T@��^@�x�@�X@�/@���@���@�z�@�r�@�bN@�1@��P@��@��R@�n�@��@�@��-@���@�p�@�G�@���@���@��j@��9@���@��@�Q�@�1@��
@��@�t�@�l�@�S�@�+@��@��@y=�@n�@h-�@a�"@Z($@R��@N=q@EF@<�u@8�Y@4�@.Ov@(�9@#C�@�s@��@��@��@G�@	e,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��HA��#A��#A��
A��;A��TA��HA��`A��yA��yA��A��A��A��A��;AԼjAԕ�A�z�A���Aӕ�A�A�oA��/A��yA�A�A̼jA���Aǧ�A�ƨA�(�A�v�A��uA�M�A�z�A��DA���A��\A��A��-A�{A�K�A�~�A�ffA��A��A��A�l�A���A�p�A�/A�M�A��hA���A�VA�oA���A��
A���A��HA���A��A�?}A�v�A���A�E�A��A��uA���A��yA���A�/A���A�ĜA�^5A�1A��DA��`A�O�A�{A�ffA���A���A�  A��A�1A�
=A���A�;dA�/A�I�A�&�A�jA�M�A�-A��;A�K�A��A~1A|bNA{l�Ay�^Aw��At��Ar�RAq�;Ap��Ao+Am�
Al{AjAh9XAf��Ae�
Ad��AdE�Ac+Aa��A_��A^��A\��AZ�RAZVAY�AY\)AXffAVv�AUXAT�jAT=qAR�/AQAQ
=AP�AP(�AOC�ANJAL�+AKƨAK�PAK\)AJ��AI��AI��AIS�AI33AH��AH�\AH9XAF�`AE�AB��AB�AA��AA?}A@ĜA@M�A?�;A>$�A;�A;��A;oA:n�A8ĜA7�
A6�RA5`BA4ĜA41A2�jA1t�A1�A0��A/�PA.jA-O�A,�A+�A+A*ffA)�#A)&�A(��A(ffA'A'�A&�+A%��A%�7A$�RA#�A#��A#C�A"I�A!&�A �AA33A��AG�A�uAz�AbNA��A?}A��A��A��A~�A=qAp�A$�AO�A��A�A�^AJA �A%AVAl�AffA
��A	x�A	"�AĜAjAK�Az�A-A��A��A�wA%A j@��@��@�1@�ȴ@�%@��@���@�P@�E�@���@���@�?}@�Q�@��@�E�@噚@�p�@�p�@�?}@�(�@�n�@�Z@ߥ�@�J@�K�@���@���@֗�@�X@��@ԓu@ӝ�@�o@҇+@ѡ�@϶F@̼j@��@�t�@���@�=q@�G�@�(�@ǶF@�@���@Ĵ9@�(�@î@¸R@��-@�7L@���@���@�ƨ@���@�+@���@���@��T@���@�ȴ@��7@���@��u@�bN@�b@��P@�l�@�K�@�o@���@��@�/@�%@�  @�t�@�@�{@�%@��@�l�@��@���@�E�@���@���@�7L@���@�1'@��@���@�n�@��@��-@��@�Q�@��@��@�$�@���@���@�7L@��D@���@�t�@��@���@�v�@���@�%@��/@��D@��@��@���@���@�\)@��@�ȴ@��H@��@��!@��\@�^5@���@���@��u@��@�&�@��`@�(�@�1@���@��@�+@�V@�E�@�V@�^5@�E�@�-@���@�x�@�V@�V@�V@�7L@�O�@�`B@�O�@�Q�@�b@��@�b@�(�@�  @�t�@�t�@�dZ@�S�@��H@�V@�$�@�-@���@�x�@�&�@���@�7L@�7L@�7L@��@��@��@��@���@���@��9@�9X@�  @��
@�|�@�33@�
=@��@��@�~�@�n�@�^5@�=q@���@��T@���@���@�p�@�7L@��@�%@�Ĝ@��@�z�@�j@�bN@�Z@�I�@�  @��@�ƨ@���@�|�@�dZ@�+@���@���@�ff@�=q@��#@��T@��^@�x�@�X@�/@���@���@�z�@�r�@�bN@�1@��P@��@��R@�n�@��@�@��-@���@�p�@�G�@���@���@��j@��9@���@��@�Q�@�1@��
@��@�t�@�l�@�S�@�+G�O�@��@y=�@n�@h-�@a�"@Z($@R��@N=q@EF@<�u@8�Y@4�@.Ov@(�9@#C�@�s@��@��@��@G�@	e,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�NB�TB�TB�NB�NB�NB�TB�TB�NB�NB�NB�NB�NB�TB�TB�B��B%BuB&�B+B1'BM�Br�B�oB�B�9B�FBƨB��B��B��B��B��B��B��B��BǮBÖB��B�qB�RB�9B�B��B��B�\B�7B�B{�Bu�Br�Bn�BffB^5BF�B@�B8RB"�B�B��B�B�HB��BŢB��B�dB�FB�B��B��B��B�VB�+B� B{�Bs�BiyBcTBI�BA�B)�B�B	7B
��B
�fB
��B
�-B
��B
��B
��B
��B
�PB
�JB
�=B
�+B
�B
{�B
t�B
iyB
^5B
W
B
L�B
=qB
1'B
(�B
"�B
�B
hB
1B	��B	�B	�mB	�NB	�)B	�
B	��B	ɺB	��B	�RB	�B	��B	��B	��B	��B	�hB	�1B	�B	{�B	x�B	r�B	k�B	ffB	dZB	aHB	\)B	W
B	M�B	I�B	G�B	F�B	C�B	?}B	>wB	<jB	;dB	:^B	7LB	49B	/B	"�B	�B	uB	hB	VB	DB	1B	%B	B��B�B�B�B�sB�NB�BB�B�B��B��BȴBǮBŢBB�wB�dB�XB�?B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�DB�=B�%B�B�B�B�B�B�B� B� B~�B|�Bz�Bw�Bt�Bs�Bn�BjBdZB_;B^5B[#BXBT�BP�BN�BL�BJ�BC�BB�BB�BB�B>wB<jB:^B8RB8RB49B33B2-B2-B2-B0!B0!B/B.B-B-B,B-B-B-B-B-B,B,B.B.B,B/B33B8RB:^B;dB@�BC�BD�BC�BC�BD�BG�BK�BL�BL�BK�BL�BL�BO�BT�BS�BXBVBW
BXBZB\)B_;BaHBcTBe`BffBm�Bp�Bn�Bn�Bo�Bq�Bs�Bu�Bv�Bv�Bw�Bw�Bx�Bx�Bx�By�Bz�B� B�B�B�%B�+B�1B�=B�PB�oB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�LB�XB�qB��B��BÖBƨB��B��B��B�B�B�)B�ZB�mB�B�B�B��B��B	B	B	%B		7B	
=B	VB	uB	�B	�B	�B	!�B	#�B	%�B	'�B	+B	-B	2-B	6FB	9XB	;dB	=qB	?}B	A�B	D�B	D�B	F�B	K�B	M�B	O�B	P�B	P�B	Q�B	[#B	^5B	aHB	e`B	jB	l�B	m�B	n�B	o�B	p�B	p�B	t�B	x�B	z�B	|�B	�B	�B	�+B	�=B	�DB	�DB	�JB	�JB	�VB	�bB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�3B	�FB	�LB	�RB	�XB	�XB	�XB	�dB	�wB	�}B	B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�wB
�B
�B
=B
$&B
,�B
3�B
<�B
CB
G+B
L�B
QhB
W$B
Z�B
`�B
dZB
h>B
m�B
r�B
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�@B�FB�FB�@B�@B�@B�FB�FB�@B�@B�@B�@B�@B�FB�FB�wB��B�B
dB�B!�B(BD�Bi�B�VB��B�B�+B��B��B��B��B��B��B��BĺB��B��B�B�mB�[B�=B�$B��B��B��B�KB�'B{	Br�Bl�Bi�Be�B]YBU)B=�B7yB/IB�B�B��B�B�FB��B��B��B�eB�GB�
B��B��B��B�[B~0BwBr�Bj�B`�BZ\B@�B8�B!	B�B GB
��B
�xB
��B
�DB
�B
��B
��B
��B
�jB
�dB
�WB
~FB
x!B
sB
k�B
`�B
USB
N)B
C�B
4�B
(IB
 B
�B
�B
�B	�WB	�B	��B	ޖB	�wB	�SB	�4B	�(B	��B	��B	�B	�IB	��B	��B	��B	��B	��B	bB	x8B	sB	pB	i�B	b�B	]�B	[�B	X|B	S^B	N?B	E	B	@�B	>�B	=�B	:�B	6�B	5�B	3�B	2�B	1�B	.�B	+qB	&TB	B	�B	
�B	�B	�B	�B�mB�bB�OB��B��B��B��B߲BَBׂB�XB�EB�4B�B��B��B��B��B��B��B��B��B�xB�lB�YB�TB�NB�HB�<B�6B�/B�#B�B�B��B��B��B��B��B��B��B��B��B��B}oB|iB|iB{cByVBxPBxPBwJBwKBvEBt9Br,BoBlBkBe�Ba�B[�BV�BU�BRrBO_BLNBH5BF)BDBBB:�B9�B9�B9�B5�B3�B1�B/�B/�B+�B*�B)�B)�B)�B'vB'vB&pB%iB$cB$dB#^B$dB$dB$dB$dB$dB#^B#^B%jB%jB#^B&qB*�B/�B1�B2�B7�B:�B;�B:�B:�B;�B?BCBD#BD#BCBD#BD#BG5BLSBKNBOeBMZBN`BOfBQrBS~BV�BX�BZ�B\�B]�Bd�Bg�Be�Be�Bf�Bh�BkBmBnBnBo$Bo$Bp*Bp*Bp*Bq0Br6BwTBxZBzfB}yB~B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�0B�CB�IB�`B��B��B��B��B��B��B��B��B��B�B�6B�HB�TB�gB�yB۩B޼B��B��B��B�B�/B�TB�fB�rB	 �B	�B	�B	
�B	�B	�B	
B	B	"B	.B	;B	"MB	$YB	)xB	-�B	0�B	2�B	4�B	6�B	8�B	;�B	;�B	=�B	CB	EB	G(B	H.B	H.B	I5B	RkB	U}B	X�B	\�B	a�B	c�B	d�B	e�B	f�B	g�B	g�B	lB	pB	r'B	t4B	xLB	|dB	~pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�,B	�9B	�?B	�EB	�EB	�QB	�dB	�jB	�pB	�vB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	� B	� B	�-B	�9B	�EB	�KB	�XB	�^B	�iB	�oB	�uB	�uB	�{B	؈B	؈B	؈B	؈B	َB	َB	ڔB	ۚB	ܠB	ݦB	ޭB	ޭB	ޭB	߳G�O�B	��B	��B
�B
�B
{B
cB
#�B
*�B
4*B
:NB
>gB
C�B
H�B
N`B
RB
W�B
[�B
_yB
d�B
j B
k@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170854    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170854  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170854  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                