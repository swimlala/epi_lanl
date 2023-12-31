CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:15Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170915  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ]A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^?��1   @��^���@6;dZ��c�M���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ]A   B   B   @�ff@�  A   A!��A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8��B?33BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�!HD�X�D��RD�� D�%�D�VfD���D��\D�$�D�UqD��
D�� D�)D�W
Dډ�D���D��D�R=D�=D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�@�z�A�A>=qA\��A~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B8\)B>BG(�BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dh\Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�)Dy��D��D�UD���D��{D�">D�R�D��3D���D�!HD�Q�D���D��{D��D�S�DچgD��qD�
D�N�D�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�33A�5?A�=qA�E�A�I�A�`BA؍PA؝�AجA�ĜA���A���A���A��#A��;A���A���Aغ^A�VA�I�A�-A�x�A�7LA�ƨA�C�A�G�A�bNA�~�A�XA���A�+A��hA�n�A��\A��7A��;A��;A�=qA��A�$�A�`BA��A�VA�E�A�ĜA�t�A���A�/A�ĜA���A�(�A��/A�bA�^5A�n�A�ĜA�hsA�VA��A���A��\A�9XA��A�C�A���A���A�z�A�%A���A��\A�C�A�/A� �A�"�A��A�7LA���A�1'A��RA��A�A�A���A��#A�I�A�S�A��A���A��wA��\A�C�A��#A�33A���A��A�A���A���A��TA�z�A�VA��A��yA��PA���A� �A���A�$�A��DA���A�x�A��/A�33A�r�A��A��A�{A|I�AzE�Ay�wAx��Au��Aq��Ap�AnbAmG�Al^5Ak�PAi\)Ag��Ae��AcS�Ab�Aal�A`5?A^��A[�mAY+AW&�AV1AU\)ATffAS�AR��AQ��AP�9AO��ANVAL�AL{AJ�AI�AG�^AG"�AF�yAF(�AE�hAE%AD�9AB�9AAhsA@�A@ffA?�;A>��A<�A;p�A:E�A8��A7x�A5��A4�yA3�;A2jA1G�A0��A/C�A.v�A-�A-\)A,�A+�A*bNA(�!A&��A%�A%�FA%G�A$�A#/A ~�A/A��An�A�hA�9A��A�A��A1AAt�AȴA�\A �Al�AJAVA�AQ�AhsA��A
ZA	33A��A�A?}A�A�AVAr�AO�A �9A bN@�|�@��@�p�@� �@�@���@��@�^5@���@�@�&�@�@�n�@��@��m@�F@�dZ@�x�@�/@��@�Q�@���@�G�@�u@�F@��@�%@�1'@��@�~�@��`@ۮ@�;d@ڸR@�@�&�@��
@�ȴ@���@��`@��;@�S�@�
=@�^5@�&�@�M�@̓u@�hs@��/@�b@���@ț�@���@��T@��`@�M�@��@�ȴ@���@��H@��7@���@��@��@�A�@��w@�K�@���@��@�33@���@��+@��-@���@��@��+@���@�G�@���@�1'@���@��R@��R@��!@�~�@���@�V@���@��j@��@�  @��
@���@�\)@��@��7@��`@�Z@�1@��w@�l�@��y@�~�@�{@��@�ff@�~�@��R@��@�l�@��P@��!@��@���@���@�@��^@��#@�%@�Z@� �@�b@��;@�dZ@�ȴ@�^5@���@�5?@��#@��h@��h@��7@�hs@���@�x�@���@��@�
=@���@��y@��H@��@��@�5?@�{@�x�@�G�@���@�j@���@��R@���@� �@�  @�n�@�@�/@�1@�$�@�{@��#@���@��@�=q@�-@�M�@�~�@��@��R@�O�@��@��D@�b@��@��@�C�@�|�@��;@���@�dZ@��+@���@��h@��7@��@�x�@��@���@�%@�`B@���@��
@��w@�ƨ@�|�@�|�@�+@���@�$�@�$�@��@�{@��^@�G�@�%@���@��@���@�bN@�b@�1@��@���@��@���@���@��P@�S�@�C�@�;d@�"�@���@�ȴ@��R@���@�n�@�^5@�=q@�-@�@�@���@�V@�V@���@��@��j@��u@�I�@�1@���@��m@���@��@�\)@�o@���@���@���@��!@�^5@�{@�@��@���@�p�@�`B@�O�@�&�@���@��9@��@�Q�@�1@��m@���@|]d@t�@m#�@`��@X�@Q�@K�@Fں@>�@:�@1�z@+��@&c @!Dg@M@(@1�@�@\)@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�/A�33A�5?A�=qA�E�A�I�A�`BA؍PA؝�AجA�ĜA���A���A���A��#A��;A���A���Aغ^A�VA�I�A�-A�x�A�7LA�ƨA�C�A�G�A�bNA�~�A�XA���A�+A��hA�n�A��\A��7A��;A��;A�=qA��A�$�A�`BA��A�VA�E�A�ĜA�t�A���A�/A�ĜA���A�(�A��/A�bA�^5A�n�A�ĜA�hsA�VA��A���A��\A�9XA��A�C�A���A���A�z�A�%A���A��\A�C�A�/A� �A�"�A��A�7LA���A�1'A��RA��A�A�A���A��#A�I�A�S�A��A���A��wA��\A�C�A��#A�33A���A��A�A���A���A��TA�z�A�VA��A��yA��PA���A� �A���A�$�A��DA���A�x�A��/A�33A�r�A��A��A�{A|I�AzE�Ay�wAx��Au��Aq��Ap�AnbAmG�Al^5Ak�PAi\)Ag��Ae��AcS�Ab�Aal�A`5?A^��A[�mAY+AW&�AV1AU\)ATffAS�AR��AQ��AP�9AO��ANVAL�AL{AJ�AI�AG�^AG"�AF�yAF(�AE�hAE%AD�9AB�9AAhsA@�A@ffA?�;A>��A<�A;p�A:E�A8��A7x�A5��A4�yA3�;A2jA1G�A0��A/C�A.v�A-�A-\)A,�A+�A*bNA(�!A&��A%�A%�FA%G�A$�A#/A ~�A/A��An�A�hA�9A��A�A��A1AAt�AȴA�\A �Al�AJAVA�AQ�AhsA��A
ZA	33A��A�A?}A�A�AVAr�AO�A �9A bN@�|�@��@�p�@� �@�@���@��@�^5@���@�@�&�@�@�n�@��@��m@�F@�dZ@�x�@�/@��@�Q�@���@�G�@�u@�F@��@�%@�1'@��@�~�@��`@ۮ@�;d@ڸR@�@�&�@��
@�ȴ@���@��`@��;@�S�@�
=@�^5@�&�@�M�@̓u@�hs@��/@�b@���@ț�@���@��T@��`@�M�@��@�ȴ@���@��H@��7@���@��@��@�A�@��w@�K�@���@��@�33@���@��+@��-@���@��@��+@���@�G�@���@�1'@���@��R@��R@��!@�~�@���@�V@���@��j@��@�  @��
@���@�\)@��@��7@��`@�Z@�1@��w@�l�@��y@�~�@�{@��@�ff@�~�@��R@��@�l�@��P@��!@��@���@���@�@��^@��#@�%@�Z@� �@�b@��;@�dZ@�ȴ@�^5@���@�5?@��#@��h@��h@��7@�hs@���@�x�@���@��@�
=@���@��y@��H@��@��@�5?@�{@�x�@�G�@���@�j@���@��R@���@� �@�  @�n�@�@�/@�1@�$�@�{@��#@���@��@�=q@�-@�M�@�~�@��@��R@�O�@��@��D@�b@��@��@�C�@�|�@��;@���@�dZ@��+@���@��h@��7@��@�x�@��@���@�%@�`B@���@��
@��w@�ƨ@�|�@�|�@�+@���@�$�@�$�@��@�{@��^@�G�@�%@���@��@���@�bN@�b@�1@��@���@��@���@���@��P@�S�@�C�@�;d@�"�@���@�ȴ@��R@���@�n�@�^5@�=q@�-@�@�@���@�V@�V@���@��@��j@��u@�I�@�1@���@��m@���@��@�\)@�o@���@���@���@��!@�^5@�{@�@��@���@�p�@�`B@�O�@�&�@���@��9@��@�Q�@�1G�O�@���@|]d@t�@m#�@`��@X�@Q�@K�@Fں@>�@:�@1�z@+��@&c @!Dg@M@(@1�@�@\)@֡111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�XB
�dB
�jB
�wB
��B
ÖB
��B
�BB
�B
�B
��BBJB�B �B-B:^B>wB>wBC�B8RBR�BdZB}�B� B|�B�+B��B��B�DB�1BbNBT�B^5Bl�B}�B��B�?B��B�TB�BB�B
=B33BR�BW
BW
BZBffBk�Bt�B�B�B�bB�JB� B}�B~�B|�By�Bw�Bt�Bp�Bl�BgmBS�BO�BN�BN�BL�BK�BJ�BK�BB�B;dB1'B�B�B�BbBbBbB	7BBB��B��B  BBBB��B�B�sB�#BĜB�!B��B�hB�Bm�BT�B-B�B
��B
�B
�B
�fB
�;B
ȴB
��B
�dB
�9B
�B
��B
��B
z�B
T�B
?}B
;dB
33B
�B
B	��B	�B	�ZB	�HB	�sB	�NB	�B	��B	��B	�FB	�'B	��B	��B	�{B	�%B	v�B	r�B	l�B	jB	dZB	`BB	^5B	VB	N�B	H�B	C�B	@�B	9XB	6FB	.B	'�B	%�B	#�B	�B	�B	�B	oB	
=B	%B	B	B��B��B�B�B�NB�;B�B��B��BȴBÖB��B�^B�9B�-B�B�B��B��B�DB�1B�B�B�B�B|�Bu�Bn�BjBe`BcTB_;B[#BVBR�BO�BM�BM�BK�BJ�BI�BG�BF�BB�BB�B@�B?}B:^B7LB2-B2-B.B)�B)�B.B.B+B+B(�B(�B)�B+B)�B+B(�B'�B(�B$�B$�B'�B'�B(�B(�B+B)�B+B+B.B,B,B.B/B1'B0!B2-B49B5?B5?B5?B6FB5?B6FB7LB9XB8RB8RB9XB9XB;dB;dB:^B9XB9XB=qB?}B9XB6FBB�BD�BF�BG�BL�BN�BO�BR�BZB\)BbNBe`Bl�Bs�B� B~�B�B�B� B~�B�B�%B�+B�+B�B�%B�B�B�B�%B�+B�1B�VB�hB��B��B��B��B��B��B��B��B�B�B�RB�qBB��B��B��B��B��B��B��B�
B�#B�)B�5B�fB�B��B��B	B	B	B	B��B��B��B��B��B	B	B	B	+B	1B	
=B		7B	1B	uB	oB	uB	�B	�B	�B	�B	!�B	%�B	,B	5?B	;dB	=qB	?}B	@�B	A�B	D�B	D�B	E�B	F�B	L�B	S�B	S�B	N�B	N�B	W
B	\)B	cTB	_;B	`BB	aHB	aHB	]/B	]/B	aHB	dZB	jB	p�B	v�B	y�B	}�B	�B	�%B	�B	�B	�B	�+B	�+B	�=B	�JB	�VB	�bB	�uB	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�-B	�3B	�?B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�}B	�}B	��B	B	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�ZB	�ZB	�`B	�`B	�`B	��B	��B
�B
�B
�B
$B
,�B
-wB
<�B
HfB
J�B
NB
S�B
Z7B
_;B
c�B
hXB
m]B
q�B
v�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
�B
�7B
װB
��B
�B
�ZB
��B�B�B/B$wB1�B5�B5�B:�B/�BJZB[�BuZBwfBtUB~�B�0B�B��B�BY�BLjBU�Bc�Bu]B��B��B��BڷBץB�tB�B*�BJNBNfBNfBQyB]�Bb�BlByeB|xB��B��BwZBuNBvTBtIBq6Bo*BlBg�Bc�B^�BKVBG=BF7BF8BD,BC&BB BC&B9�B2�B(�B!BB�B�B�B�B �B�kB�wB�`B�GB�fB�xB�B�lB�GB�B��BҌB�B��B�B��By|BeBLqB$�BB
�[B
�*B
�B
��B
ַB
�2B
�B
��B
��B
��B
�dB
�B
reB
L�B
7B
2�B
*�B
=B	��B	�JB	�B	��B	��B	�B	��B	ϡB	�YB	�B	��B	��B	��B	�TB	�B	}�B	nbB	jJB	d%B	bB	[�B	W�B	U�B	M�B	FvB	@QB	;4B	8!B	0�B	-�B	%�B	�B	�B	wB	YB	GB	;B	
B	�B��B��B��B��B�sB�=B�$B��B��BͫBɔB�{B�]B�?B�,B�B��B��B��B��B�wB�FB��B�B|�B{�Bz�Bx�Bt�BmsBfIBb0B]B[BV�BR�BM�BJ�BG�BE�BE�BC{BBuBAnB?cB>]B:DB:DB89B73B2B/B)�B)�B%�B!�B!�B%�B%�B"�B"�B �B �B!�B"�B!�B"�B �B�B �B�B�B�B�B �B �B"�B!�B"�B"�B%�B#�B#�B%�B&�B(�B'�B)�B+�B,�B,�B,�B. B,�B.B/B1B0B0B1B1B3B3B2B1B1B5,B78B1B.B:JB<WB>cB?iBD�BF�BG�BJ�BQ�BS�BZB]BdDBkoBw�Bv�Bx�Bx�Bw�Bv�Bx�B}�B~�B~�Bz�B}�B|�B|�B|�B}�B~�B�B�B� B�DB�WB�WB�]B�oB�oB��B��B��B��B�B�&B�DB�uB�|B�vBĂBƍBșBʦBξB��B��B��B�B�PB�uB�B��B��B��B��B��B��B�B�B��B��B��B��B��B��B	�B	 �B��B	&B	
 B	&B	2B	2B	DB	hB	{B	�B	#�B	,�B	3B	5B	7+B	81B	97B	<JB	<JB	=PB	>VB	DzB	K�B	K�B	F�B	F�B	N�B	S�B	[ B	V�B	W�B	X�B	X�B	T�B	T�B	X�B	\B	b+B	hOB	ntB	q�B	u�B	y�B	}�B	z�B	y�B	{�B	~�B	~�B	��B	��B	� B	�B	�B	�*B	�$B	�B	�0B	�6B	�6B	�<B	�<B	�OB	�UB	�nB	�gB	�[B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�$B	�$B	�*B	�6B	�=B	�IB	�IB	�IB	�OB	�OB	�UB	�[B	�aB	�hB	�hB	�hB	�sB	�B	ɒB	ʘB	˞B	̤B	ͪB	ΰB	϶B	϶B	϶B	нB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�G�O�B	�B	�UB
NB
mB
�B
�B
$cB
%B
4[B
@B
B�B
E�B
K�B
Q�B
V�B
[DB
_�B
d�B
i0B
nNB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200619170915    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170915  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170915  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                