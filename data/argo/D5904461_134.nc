CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:27Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125827  20190408133245  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @�����ix1   @���DD]�@5AG�z��c1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�fD�I�D�� D���D��fD�L�D���D�ɚD��D�FfD�vfDǙ�D��D�@ D�y�D��3D��fD�P D�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�Bp�RBxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'��D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt~�Dy�D��D�L)D���D��)D���D�O\D��\D��)D�\D�H�D�x�Dǜ)D�\D�B�D�|)D���D���D�R�D�o\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�C�A�E�A�;dA�5?A�5?A�7LA�;dA�?}A�?}A�A�A�?}A�=qA�;dA�=qA�9XA�(�A��A�oA�JA�
=A��
A�r�A�z�A�JAʲ-A�7LAǋDA��
A�1'A��TA��Aĩ�A�=qA��HA�9XA���A���A�-A��yA�ƨA�"�A��FA��\A���A�A�A���A�bNA�"�A��A�`BA��wA���A��A�XA�ȴA�VA�7LA�ZA�G�A���A��9A���A�K�A���A�ĜA�S�A�%A��hA��^A��HA���A��A�`BA�;dA�A�A���A�I�A��A��9A�E�A�G�A�`BA��FA��A���A��^A���A�A�A�p�A�M�A�9XA�-A�z�A��^A�?}A��A���A���A��A���A�XA�t�A�A��A���A���A�A|�+Az��Ay��Ax��AwC�Au;dAr{Ap�yAoƨAk�FAg��Ae%Ac%A`E�A\bNAX~�AU��AT-AQ�ANjAMG�AK"�AIoAGAEƨAE�AB��AB�AA��A@Q�A>1A<v�A;l�A9�A8Q�A7�A5��A4VA2�uA1��A0�/A0v�A/��A.�HA-�A+|�A*��A*1'A)��A)�PA(Q�A'�A'K�A&�A&�A%ƨA#��A"(�A �A�A�A��AK�A��Ax�A�AXAv�AS�A�/A�DA  A&�A-A+AE�A�^A�`AQ�A�AhsA�9A��A
�A
�DA	�;A	C�Av�AhsAȴAA+A�HA �A��AM�At�A �/A A�@�=q@�@�ff@��@��+@��-@��
@��@��@��H@��@�33@�-@�`B@��@�E�@�ƨ@�o@�?}@���@�G�@�@��#@�ȴ@��@��@��@��@�p�@�V@�Ĝ@��@�9X@�dZ@�+@��y@ڇ+@ّh@�r�@�Z@�;d@֧�@ղ-@Լj@��#@�v�@��y@�x�@�dZ@�7L@ѡ�@ҧ�@҇+@���@҇+@��@ёh@���@�z�@�1@ϕ�@�
=@��@�O�@��/@���@̼j@̼j@̬@�Q�@��@���@��m@��m@��m@ˍP@�@�V@���@��@��m@��@��m@Ǯ@ư!@�{@��T@�/@���@�r�@þw@�C�@���@�@�~�@�ff@��@���@��@��@�Q�@� �@��
@��w@��\@���@��^@��@�?}@��@��9@�j@��
@��-@���@��@�;d@�v�@�~�@�n�@���@�?}@���@��F@�5?@���@�{@���@���@��@�Z@�(�@�ƨ@�\)@��@�33@�ff@�@�X@�7L@�7L@�O�@���@�`B@��@��@�1'@���@�t�@���@�=q@��/@��@���@���@��@���@�9X@�1'@���@��P@�C�@��@���@��!@�{@�%@���@��H@��R@���@��\@��@�-@�V@��@�G�@��`@���@�&�@���@��@�V@��j@�I�@��@��;@���@�v�@���@�%@���@���@�I�@�b@�Q�@�A�@�  @���@�l�@�\)@�K�@�o@���@�E�@��^@���@��h@�X@�&�@���@�A�@�1'@��@��@��@��
@�K�@�
=@�ȴ@�n�@���@��h@�`B@�&�@�%@�Q�@��F@�t�@�33@��@���@�{@���@�@��@��@��-@��7@�p�@�`B@�/@��@��@�j@�Q�@��@�t�@���@��!@���@�ff@�ff@�M�@�5?@�$�@��@���@��T@���@���@�?}@��@��@�%@���@�Q�@�A�@�A�@� �@��w@���@��P@�\)@�33@�
=@���@��R@��R@�~�@��^@�5?@�%@yX@o�@d�j@[@R��@M��@E�@@1'@9&�@2�@+�@&ff@!%@��@�;@9X@|�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�A�A�C�A�E�A�;dA�5?A�5?A�7LA�;dA�?}A�?}A�A�A�?}A�=qA�;dA�=qA�9XA�(�A��A�oA�JA�
=A��
A�r�A�z�A�JAʲ-A�7LAǋDA��
A�1'A��TA��Aĩ�A�=qA��HA�9XA���A���A�-A��yA�ƨA�"�A��FA��\A���A�A�A���A�bNA�"�A��A�`BA��wA���A��A�XA�ȴA�VA�7LA�ZA�G�A���A��9A���A�K�A���A�ĜA�S�A�%A��hA��^A��HA���A��A�`BA�;dA�A�A���A�I�A��A��9A�E�A�G�A�`BA��FA��A���A��^A���A�A�A�p�A�M�A�9XA�-A�z�A��^A�?}A��A���A���A��A���A�XA�t�A�A��A���A���A�A|�+Az��Ay��Ax��AwC�Au;dAr{Ap�yAoƨAk�FAg��Ae%Ac%A`E�A\bNAX~�AU��AT-AQ�ANjAMG�AK"�AIoAGAEƨAE�AB��AB�AA��A@Q�A>1A<v�A;l�A9�A8Q�A7�A5��A4VA2�uA1��A0�/A0v�A/��A.�HA-�A+|�A*��A*1'A)��A)�PA(Q�A'�A'K�A&�A&�A%ƨA#��A"(�A �A�A�A��AK�A��Ax�A�AXAv�AS�A�/A�DA  A&�A-A+AE�A�^A�`AQ�A�AhsA�9A��A
�A
�DA	�;A	C�Av�AhsAȴAA+A�HA �A��AM�At�A �/A A�@�=q@�@�ff@��@��+@��-@��
@��@��@��H@��@�33@�-@�`B@��@�E�@�ƨ@�o@�?}@���@�G�@�@��#@�ȴ@��@��@��@��@�p�@�V@�Ĝ@��@�9X@�dZ@�+@��y@ڇ+@ّh@�r�@�Z@�;d@֧�@ղ-@Լj@��#@�v�@��y@�x�@�dZ@�7L@ѡ�@ҧ�@҇+@���@҇+@��@ёh@���@�z�@�1@ϕ�@�
=@��@�O�@��/@���@̼j@̼j@̬@�Q�@��@���@��m@��m@��m@ˍP@�@�V@���@��@��m@��@��m@Ǯ@ư!@�{@��T@�/@���@�r�@þw@�C�@���@�@�~�@�ff@��@���@��@��@�Q�@� �@��
@��w@��\@���@��^@��@�?}@��@��9@�j@��
@��-@���@��@�;d@�v�@�~�@�n�@���@�?}@���@��F@�5?@���@�{@���@���@��@�Z@�(�@�ƨ@�\)@��@�33@�ff@�@�X@�7L@�7L@�O�@���@�`B@��@��@�1'@���@�t�@���@�=q@��/@��@���@���@��@���@�9X@�1'@���@��P@�C�@��@���@��!@�{@�%@���@��H@��R@���@��\@��@�-@�V@��@�G�@��`@���@�&�@���@��@�V@��j@�I�@��@��;@���@�v�@���@�%@���@���@�I�@�b@�Q�@�A�@�  @���@�l�@�\)@�K�@�o@���@�E�@��^@���@��h@�X@�&�@���@�A�@�1'@��@��@��@��
@�K�@�
=@�ȴ@�n�@���@��h@�`B@�&�@�%@�Q�@��F@�t�@�33@��@���@�{@���@�@��@��@��-@��7@�p�@�`B@�/@��@��@�j@�Q�@��@�t�@���@��!@���@�ff@�ff@�M�@�5?@�$�@��@���@��T@���@���@�?}@��@��@�%@���@�Q�@�A�@�A�@� �@��w@���@��P@�\)@�33@�
=@���@��R@��R@�~�@��^@�5?@�%@yX@o�@d�j@[@R��@M��@E�@@1'@9&�@2�@+�@&ff@!%@��@�;@9X@|�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
I�B
I�B
G�B
E�B
@�B
?}B
J�B
dZB
p�B
� B
��B
��B
��B
�B
�)B,B�uB�{BƨB��BDB%B\B�B?}BM�BiyB��BǮB��BɺBǮB��B�ZB�B�B��BB+B%BB��B�yB�
B�XB�PB[#BQ�B:^B&�B�B�B{B�B�B)�BI�B_;BXBJ�B5?B�B��B�;BȴB��B�%Bv�BffB8RB
��B
�B
��B
��B
��B
��B
�FB
�!B
��B
��B
��B
�PB
�%B
v�B
`BB
N�B
B�B
9XB
!�B

=B	��B	��B	�B	�HB	��B	�qB	�3B	��B	�7B	o�B	^5B	O�B	@�B	)�B	oB	B��B�B�NB�HB�B��B��BÖB�jB�3B�B�B��B��B��B�{B�{B�oB�PB�+B�B�%B�B}�B�B~�B~�B}�Bw�Bs�Bq�Bu�By�B}�B�B�B�B�7B��B�JB�=B�Bm�BcTBbNBaHB_;B\)B\)B[#B\)BbNBbNBaHBhsBhsBe`BcTBdZBe`Bk�Bn�Bp�Bt�Bs�Br�Bv�Bw�Bu�Bs�Bs�Bs�Bq�Bn�Bm�Bo�Bp�Bl�BjBffBdZBaHBdZBiyBs�BhsBffBe`BiyB�B�B~�Bw�Bt�Bt�Bs�Br�Bq�Bs�Bu�Bp�BiyBjBy�B�Bx�Br�Bk�Bt�Bz�B{�B�B�1B�bB��B��B��B�B�-B�3B�FBĜBÖBĜBĜBŢB��B�B�ZB�NB�5B�B�`B��B��B	B	1B		7B	PB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	,B	.B	1'B	5?B	5?B	7LB	8RB	<jB	;dB	;dB	<jB	=qB	?}B	@�B	@�B	B�B	C�B	I�B	L�B	M�B	N�B	S�B	VB	W
B	XB	ZB	[#B	]/B	]/B	_;B	`BB	`BB	_;B	^5B	_;B	`BB	aHB	bNB	dZB	dZB	ffB	gmB	hsB	ffB	jB	k�B	jB	k�B	p�B	q�B	r�B	q�B	p�B	n�B	l�B	n�B	t�B	t�B	w�B	~�B	~�B	� B	~�B	~�B	� B	�B	�B	�B	�B	�+B	�7B	�=B	�\B	�oB	�{B	��B	�{B	��B	��B	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�?B	�3B	�?B	�9B	�3B	�-B	�'B	�'B	�'B	�9B	�?B	�?B	�FB	�RB	�RB	�RB	�XB	�dB	�qB	��B	��B	��B	��B	ÖB	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�;B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
DB
hB
�B
�B
$�B
.B
49B
=qB
B�B
I�B
N�B
S�B
[#B
^5B
cTB
gmB
k�B
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
I�B
I�B
G�B
E�B
@|B
?vB
J�B
dSB
p�B
�B
��B
��B
��B
�B
�#B,B�nB�wBƠB��B:BBVB�B?uBM�BisB��BǦB��BɳBǧB��B�QB�xB�B��BB%BB �B��B�oB�B�QB�GB[BQ�B:XB&�B�BzBuB~B�B)�BI�B_5BXBJ�B58B�B��B�3BȮB��B�Bv�Bf^B8LB
��B
�B
ʷB
��B
��B
��B
�@B
�B
��B
��B
��B
�FB
�B
v�B
`;B
N�B
B�B
9RB
!�B

7B	��B	��B	�B	�AB	��B	�jB	�*B	��B	�/B	o�B	^.B	O�B	@{B	)�B	eB	B��B�vB�FB�AB�B��BʺBÎB�bB�*B�B� B��B��B��B�rB�sB�gB�FB�!B�B�B��B}�B��B~�B~�B}�Bw�Bs�Bq�Bu�By�B}�B�B�
B�B�0B��B�@B�5B�Bm�BcJBbEBaBB_3B\!B\B[B\BbEBbGBa=BhiBhiBeYBcJBdSBeWBk}Bn�Bp�Bt�Bs�Br�Bv�Bw�Bu�Bs�Bs�Bs�Bq�Bn�Bm�Bo�Bp�Bl�BjuBf]BdQBa?BdOBinBs�BhhBf]BeUBipB�B�B~�Bw�Bt�Bt�Bs�Br�Bq�Bs�Bu�Bp�BioBjvBy�B�Bx�Br�Bk|Bt�Bz�B{�B��B�%B�XB�}B��B��B�B� B�)B�9BđBÊBēBĒBŖB��B�B�PB�DB�*B�B�ZB��B��B	B	)B		-B	FB	cB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	+�B	.B	1B	55B	58B	7DB	8JB	<^B	;XB	;[B	<_B	=fB	?sB	@zB	@zB	B�B	C�B	I�B	L�B	M�B	N�B	S�B	U�B	W B	XB	ZB	[B	]%B	]%B	_0B	`7B	`8B	_1B	^+B	_1B	`6B	a@B	bDB	dOB	dNB	fYB	gcB	hkB	f\B	juB	kzB	jxB	kzB	p�B	q�B	r�B	q�B	p�B	n�B	l�B	n�B	t�B	t�B	w�B	~�B	~�B	�B	~�B	~�B	�B	�B	�B	�B	�B	�!B	�,B	�2B	�RB	�fB	�sB	�wB	�oB	�xB	��B	��B	�~B	�lB	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�
B	�#B	�(B	�,B	�0B	�4B	�5B	�(B	�2B	�.B	�+B	�"B	�B	�B	�B	�0B	�8B	�5B	�=B	�HB	�GB	�HB	�PB	�YB	�hB	�zB	�}B	�B	�B	ÌB	ÌB	řB	řB	ƞB	ǣB	ȪB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�+B	�,B	�0B	�-B	�8B	�7B	�2B	�(B	�*B	�1B	�6B	�=B	�DB	�DB	�IB	�HB	�JB	�PB	�QB	�OB	�TB	�[B	�bB	�iB	�iB	�pB	�gB	�jB	�oB	�nB	�pB	�tB	�|B	�B	�B	�B	�B	�B	�B	�B	��B
B
;B
^B
�B
�B
$�B
.
B
4.B
=eB
B�B
I�B
N�B
S�B
[B
^,B
cKB
gcB
k|B
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.08 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332452019040813324520190408133245  AO  ARCAADJP                                                                    20181121125827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133245  IP                  G�O�G�O�G�O�                