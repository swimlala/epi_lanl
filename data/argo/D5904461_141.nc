CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:29Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125829  20190408133247  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @���|5.O1   @���&�@4��`A�7�b�j~��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�3D�3D�FfD�|�D�ɚD� D�&fD�� D�ɚD��D�C3D��3DǬ�D�3D�<�D�l�D���D���D�@ D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBI�BP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)Bԏ\B؏\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.CG�CG�C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtxRDy��D��D�L)D���D��]D��D�,)D���D��]D��D�H�D���Dǲ�D��D�B�D�r�D�ҐD��D�E�D�x�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aδ9Aδ9AζFAδ9Aδ9Aδ9Aδ9Aβ-Aβ-Aβ-AΛ�A�l�A�K�A�(�A�
=A��`A��HA���A���A�ƨAͼjA͕�A�M�A���A���A�A̴9A̗�A�x�A�dZA�5?A��A�|�A���AʅA�ƨA�{A�+A�hsA���A��A�(�A�  A���A��A�A���A�`BA�/A���A���A��#A��FA�VA�G�A��RA���A�p�A�{A���A�S�A��HA���A�O�A�n�A��A�VA�|�A�9XA��hA��yA���A��+A���A��A���A�ZA��
A�l�A�  A� �A�5?A�C�A�ZA���A��A�ZA�\)A��A���A�x�A�VA���A��A� �A��A�dZA�-A�~�A���A�hsA���A��HA���A��A��/A�n�A�x�A���A��A�VA�A~��A{XAyK�Aw�FAt1Ap �Al��Ai�Ad��AdA�Ad9XAd=qAbA�A_�-A]ƨAZ�!AX��AX~�AX(�AV{AT(�AQ`BAP(�AO+AL�yAJffAH(�AD5?AC;dAA��A?��A>1A<r�A8��A7|�A6��A5��A3C�A0��A/ƨA-��A,ȴA+p�A*�A)�A'/A&A%?}A#�A!��A�AK�AƨAQ�AAQ�AO�A�FA^5AA�A�AQ�A$�A�
A
=A�HAt�A�AG�A\)A
�+A	G�A�^A�
A�+A�wA��A�FAx�A�AJA��A7LAoA�!A�A`BA ��A �9A v�A {@�33@�G�@��m@��@�=q@���@���@��
@��R@�$�@�1'@���@�t�@홚@� �@�l�@�^5@���@�  @�C�@�S�@�t�@�l�@�S�@��@�@��
@�;d@�!@���@�V@��`@�D@�I�@��;@�C�@��@�~�@�-@�`B@�b@ߥ�@�|�@�K�@�n�@�/@�9X@�  @�1@�ƨ@�v�@���@ؼj@��m@�o@�^5@Ձ@�G�@�/@��`@Լj@�z�@��@ёh@�z�@ϝ�@�Z@�A�@�\)@��H@�ȴ@�^5@�X@�bN@ʸR@�p�@Ȭ@�b@�-@�33@��y@ɑh@��/@��@���@�I�@�{@���@�@ŉ7@���@�Z@�dZ@�E�@�$�@��@��@��h@�x�@�dZ@�v�@���@���@��`@�r�@��
@�ȴ@�M�@���@��@�r�@��@��@�S�@�33@�o@��@�v�@�$�@�G�@���@�Q�@��F@��@�l�@�S�@�33@���@��!@�^5@��@���@��@���@���@���@�r�@�9X@�b@�  @��@��@�"�@���@�ff@�-@��@��^@���@�X@�&�@���@���@�Q�@� �@��
@�dZ@�@��!@��y@�ȴ@���@�~�@�V@��#@���@�p�@�X@�?}@��j@��@�j@�A�@�1@�ƨ@�33@�ȴ@���@�V@�J@��@��h@�7L@��@�Z@��@�ȴ@��y@�E�@�hs@�O�@���@��j@���@�{@�=q@�@�hs@�?}@��#@��^@��D@�ƨ@���@�@�x�@�/@�Ĝ@��@�Z@�I�@�A�@�r�@�Z@�I�@���@�C�@�dZ@�33@�+@�
=@��H@��R@�v�@�{@��-@���@��h@��@�`B@�V@���@���@�1'@��@�1@��m@��@��@���@��F@��w@�ƨ@��@�;d@��y@��+@��^@�`B@��@��@��D@�I�@��@��;@�l�@�33@�o@��@��@��\@�n�@�n�@�^5@��@��7@��@�x�@�hs@�/@���@��u@� �@���@���@�K�@��@��H@��!@�v�@�=q@��@��T@���@���@��D@�K�@|9X@pQ�@hbN@a��@[33@TI�@Nff@F5?@@Q�@9hs@2�\@+�F@&��@ ��@Z@;d@I�@1'@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aδ9Aδ9AζFAδ9Aδ9Aδ9Aδ9Aβ-Aβ-Aβ-AΛ�A�l�A�K�A�(�A�
=A��`A��HA���A���A�ƨAͼjA͕�A�M�A���A���A�A̴9A̗�A�x�A�dZA�5?A��A�|�A���AʅA�ƨA�{A�+A�hsA���A��A�(�A�  A���A��A�A���A�`BA�/A���A���A��#A��FA�VA�G�A��RA���A�p�A�{A���A�S�A��HA���A�O�A�n�A��A�VA�|�A�9XA��hA��yA���A��+A���A��A���A�ZA��
A�l�A�  A� �A�5?A�C�A�ZA���A��A�ZA�\)A��A���A�x�A�VA���A��A� �A��A�dZA�-A�~�A���A�hsA���A��HA���A��A��/A�n�A�x�A���A��A�VA�A~��A{XAyK�Aw�FAt1Ap �Al��Ai�Ad��AdA�Ad9XAd=qAbA�A_�-A]ƨAZ�!AX��AX~�AX(�AV{AT(�AQ`BAP(�AO+AL�yAJffAH(�AD5?AC;dAA��A?��A>1A<r�A8��A7|�A6��A5��A3C�A0��A/ƨA-��A,ȴA+p�A*�A)�A'/A&A%?}A#�A!��A�AK�AƨAQ�AAQ�AO�A�FA^5AA�A�AQ�A$�A�
A
=A�HAt�A�AG�A\)A
�+A	G�A�^A�
A�+A�wA��A�FAx�A�AJA��A7LAoA�!A�A`BA ��A �9A v�A {@�33@�G�@��m@��@�=q@���@���@��
@��R@�$�@�1'@���@�t�@홚@� �@�l�@�^5@���@�  @�C�@�S�@�t�@�l�@�S�@��@�@��
@�;d@�!@���@�V@��`@�D@�I�@��;@�C�@��@�~�@�-@�`B@�b@ߥ�@�|�@�K�@�n�@�/@�9X@�  @�1@�ƨ@�v�@���@ؼj@��m@�o@�^5@Ձ@�G�@�/@��`@Լj@�z�@��@ёh@�z�@ϝ�@�Z@�A�@�\)@��H@�ȴ@�^5@�X@�bN@ʸR@�p�@Ȭ@�b@�-@�33@��y@ɑh@��/@��@���@�I�@�{@���@�@ŉ7@���@�Z@�dZ@�E�@�$�@��@��@��h@�x�@�dZ@�v�@���@���@��`@�r�@��
@�ȴ@�M�@���@��@�r�@��@��@�S�@�33@�o@��@�v�@�$�@�G�@���@�Q�@��F@��@�l�@�S�@�33@���@��!@�^5@��@���@��@���@���@���@�r�@�9X@�b@�  @��@��@�"�@���@�ff@�-@��@��^@���@�X@�&�@���@���@�Q�@� �@��
@�dZ@�@��!@��y@�ȴ@���@�~�@�V@��#@���@�p�@�X@�?}@��j@��@�j@�A�@�1@�ƨ@�33@�ȴ@���@�V@�J@��@��h@�7L@��@�Z@��@�ȴ@��y@�E�@�hs@�O�@���@��j@���@�{@�=q@�@�hs@�?}@��#@��^@��D@�ƨ@���@�@�x�@�/@�Ĝ@��@�Z@�I�@�A�@�r�@�Z@�I�@���@�C�@�dZ@�33@�+@�
=@��H@��R@�v�@�{@��-@���@��h@��@�`B@�V@���@���@�1'@��@�1@��m@��@��@���@��F@��w@�ƨ@��@�;d@��y@��+@��^@�`B@��@��@��D@�I�@��@��;@�l�@�33@�o@��@��@��\@�n�@�n�@�^5@��@��7@��@�x�@�hs@�/@���@��u@� �@���@���@�K�@��@��H@��!@�v�@�=q@��@��T@���@���@��D@�K�@|9X@pQ�@hbN@a��@[33@TI�@Nff@F5?@@Q�@9hs@2�\@+�F@&��@ ��@Z@;d@I�@1'@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B)�B)�B+B+B+B+B+B+B+B-B.B0!B2-B33B2-B2-B2-B49B6FB?}BG�BI�BI�BI�BI�BJ�BK�BS�BaHBhsBe`Bm�Bx�B�B�VB�B��B�`B��B��BPB&�B9XBN�BH�BM�BP�BT�Bn�B�PB��B��B�!B�B��B��B��B��B��B��B��B��B��B�dB�RB��B��B��B��B��B��B~�B^5BT�BD�B33B#�B%B��B�B�ZB��BȴB�XB��B��B�PB�B�Bz�Bs�BiyBT�BH�B-B
��B
�;B
�wB
��B
�PB
}�B
l�B
W
B
-B

=B	��B	�#B	��B	�5B	�sB	�HB	ŢB	�LB	��B	�+B	gmB	R�B	6FB	{B	VB	�B	=qB	H�B	=qB	1'B	"�B	�B	�B	�B	VB	B��B�B�yB�)B��B�}B��B��B��B�7B~�Bv�Bn�Bt�Bv�Bv�Bs�Bs�Bs�Bt�Bq�Bq�Bq�Bo�Bl�BhsBe`B^5BR�BO�BR�BL�BI�BH�BG�BE�BC�BB�BA�B?}BA�BD�BF�BD�BD�B<jB6FB;dB49B/B,B)�B5?BG�BH�BC�BB�BF�BN�BT�BT�BT�BT�BT�BW
BXBZBdZBk�Bp�Bt�Bp�Bn�Bo�Br�Bq�Bv�B|�Bz�B{�Bw�Bq�Bm�BjBiyBjBl�Bm�Bo�B{�B�B�B�%B�+B�bB��B��B��B��B�B�B�B�B�B�B�3B�LB�LB�RB�dB�}B��B��B��B��BBBÖBŢBƨBǮBƨBŢBŢBŢBƨBȴBɺB��B��B��B�B�
B��B��B��B�
B�#B�/B�5B�;B�BB�;B�)B�B�B�;B�TB��B	+B	1B		7B	JB	VB	VB	JB	1B	\B	{B	uB	hB	VB	VB	\B	hB	oB	oB	�B	�B	�B	�B	�B	 �B	"�B	&�B	$�B	#�B	!�B	!�B	!�B	$�B	+B	5?B	7LB	9XB	;dB	<jB	>wB	@�B	?}B	A�B	E�B	L�B	P�B	Q�B	R�B	W
B	XB	\)B	`BB	bNB	cTB	ffB	jB	k�B	m�B	o�B	p�B	q�B	q�B	r�B	s�B	v�B	y�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�+B	�7B	�=B	�7B	�=B	�DB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�jB	�jB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�#B	�)B	�/B	�5B	�BB	�BB	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
VB
�B
!�B
'�B
.B
49B
7LB
>wB
A�B
I�B
P�B
W
B
\)B
`BB
e`B
jB
l�B
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B)�B)�B)�B)�B*�B*�B*�B*�B*�B*�B*�B,�B.B0B2B3#B2B2 B2B4*B68B?mBG�BI�BI�BI�BI�BJ�BK�BS�Ba;BheBeRBm�Bx�B�B�FB��B��B�QB��B��B@B&�B9GBN�BH�BM�BP�BT�Bn�B�?B��B��B�B�B��B��B��B��B��B��B��B��B��B�RB�BB��B��B��B��B��B��B~�B^$BT�BD�B3!B#�BB��B�B�JB��BȥB�HB��B�wB�@B�B��Bz�Bs�BihBT�BH�B,�B
��B
�)B
�gB
��B
�>B
}�B
lyB
V�B
,�B

(B	��B	�B	��B	�&B	�`B	�4B	őB	�6B	��B	�B	g\B	R�B	63B	gB	DB	�B	=]B	H�B	=[B	1B	"�B	�B	�B	�B	CB	B��B�B�gB�B��B�hB��B��B�mB�#B~�Bv�Bn�Bt�Bv�Bv�Bs�Bs�Bs�Bt�Bq�Bq�Bq�Bo�BlvBh]BeLB^BR�BO�BR�BL�BI�BH�BG�BE�BC�BBzBAuB?fBAtBD�BF�BD�BD�B<TB61B;NB4&B/B+�B)�B5(BG�BH�BC�BByBF�BN�BT�BT�BT�BT�BT�BV�BW�BZBdBBknBp�Bt�Bp�Bn�Bo�Br�Bq�Bv�B|�Bz�B{�Bw�Bq�Bm{BjhBibBjiBluBm}Bo�B{�B��B�B�B�B�MB��B��B��B��B��B��B��B��B��B��B�B�6B�5B�:B�MB�fB�mB�oB�jB�sB�xB�{BÀBŋBƑBǖBƓBŊBŊBŉBƒBȜBɣBʫB̵B��B��B��B��B��B��B��B�B�B� B�$B�+B�$B�B�B��B�%B�<B��B	B	B		!B	2B	AB	@B	1B	B	FB	cB	\B	QB	?B	?B	FB	SB	ZB	WB	�B	�B	�B	vB	�B	 �B	"�B	&�B	$�B	#�B	!�B	!�B	!�B	$�B	*�B	5&B	75B	9?B	;OB	<RB	>_B	@mB	?gB	AqB	E�B	L�B	P�B	Q�B	R�B	V�B	W�B	\B	`+B	b8B	c;B	fNB	jfB	knB	m|B	o�B	p�B	q�B	q�B	r�B	s�B	v�B	y�B	{�B	|�B	~�B	�B	�B	��B	��B	�	B	�B	�B	�B	�$B	�B	�&B	�-B	�?B	�ZB	�cB	�qB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�vB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�(B	�0B	�4B	�;B	�@B	�MB	�UB	�QB	�TB	�ZB	�`B	�eB	�lB	�mB	�vB	�}B	ăB	ŉB	ƑB	ʩB	ͻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�*B	�*B	�5B	�6B	�=B	�BB	�GB	�OB	�SB	�[B	�fB	�mB	�mB	�uB	�qB	�zB	�zB	�xB	�B	�B	��B	�B	�B	��B
B
=B
�B
!�B
'�B
-�B
4!B
76B
>`B
ApB
I�B
P�B
V�B
\B
`+B
eJB
jgB
lrB
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332472019040813324720190408133247  AO  ARCAADJP                                                                    20181121125829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125829  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133247  IP                  G�O�G�O�G�O�                